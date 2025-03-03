// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use nix::sys::epoll::Epoll;
use nix::sys::epoll::EpollCreateFlags;
use nix::sys::epoll::EpollEvent;
use nix::sys::epoll::EpollFlags;
use nix::sys::epoll::EpollTimeout;
use nix::sys::eventfd::EventFd;

use std::collections::VecDeque;
use std::marker::PhantomData;
use std::os::fd::AsFd;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use crate::c2_wrapper::C2EncodeJob;
use crate::c2_wrapper::C2State;
use crate::c2_wrapper::C2Status;
use crate::c2_wrapper::C2Worker;
use crate::c2_wrapper::DrainMode;
use crate::c2_wrapper::Job;
use crate::decoder::StreamInfo;
use crate::encoder::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::RateControl::ConstantBitrate;
use crate::encoder::RateControl::ConstantQuality;
use crate::encoder::Tunings;
use crate::encoder::VideoEncoder;
use crate::image_processing::convert_video_frame;
use crate::image_processing::extend_border_nv12;
#[cfg(feature = "v4l2")]
use crate::video_frame::V4l2VideoFrame;
use crate::video_frame::VideoFrame;
use crate::video_frame::UV_PLANE;
use crate::video_frame::Y_PLANE;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;
use crate::Resolution;

pub trait C2EncoderBackend {
    type EncoderOptions: Clone + Send + 'static;

    fn new(options: Self::EncoderOptions) -> Result<Self, String>
    where
        Self: Sized;

    // Returns a new encoder, the visible resolution, and the coded resolution.
    #[cfg(feature = "vaapi")]
    fn get_encoder<V: VideoFrame>(
        &mut self,
        input_format: DecodedFormat,
        output_format: EncodedFormat,
    ) -> Result<(Box<dyn VideoEncoder<V>>, Resolution, Resolution), String>;
    #[cfg(feature = "v4l2")]
    fn get_encoder<V: VideoFrame>(
        &mut self,
        input_format: DecodedFormat,
        output_format: EncodedFormat,
    ) -> Result<(Box<dyn VideoEncoder<V4l2VideoFrame<V>>>, Resolution, Resolution), String>;
}

pub struct C2EncoderWorker<V, B>
where
    V: VideoFrame,
    B: C2EncoderBackend,
{
    #[cfg(feature = "vaapi")]
    encoder: Box<dyn VideoEncoder<V>>,
    #[cfg(feature = "v4l2")]
    encoder: Box<dyn VideoEncoder<V4l2VideoFrame<V>>>,
    awaiting_job_event: Arc<EventFd>,
    error_cb: Arc<Mutex<dyn FnMut(C2Status) + Send + 'static>>,
    work_done_cb: Arc<Mutex<dyn FnMut(C2EncodeJob<V>) + Send + 'static>>,
    alloc_cb: Arc<Mutex<dyn FnMut() -> Option<V> + Send + 'static>>,
    work_queue: Arc<Mutex<VecDeque<C2EncodeJob<V>>>>,
    in_flight_queue: VecDeque<C2EncodeJob<V>>,
    state: Arc<Mutex<C2State>>,
    current_tunings: Tunings,
    visible_resolution: Resolution,
    coded_resolution: Resolution,
    _phantom: PhantomData<B>,
}

impl<V, B> C2EncoderWorker<V, B>
where
    V: VideoFrame,
    B: C2EncoderBackend,
{
    fn poll_complete_frames(&mut self) {
        loop {
            match self.encoder.poll() {
                Ok(Some(coded)) => {
                    let mut job = self.in_flight_queue.pop_front().unwrap();
                    job.output = coded.bitstream;
                    (*self.work_done_cb.lock().unwrap())(job);
                }
                Err(err) => {
                    log::debug!("Error during encode! {:?}", err);
                    *self.state.lock().unwrap() = C2State::C2Error;
                    (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                    break;
                }
                _ => break,
            }
        }
    }
}

impl<V, B> C2Worker<C2EncodeJob<V>> for C2EncoderWorker<V, B>
where
    V: VideoFrame,
    B: C2EncoderBackend,
{
    type Options = B::EncoderOptions;

    fn new(
        input_fourcc: Fourcc,
        output_fourcc: Fourcc,
        awaiting_job_event: Arc<EventFd>,
        error_cb: Arc<Mutex<dyn FnMut(C2Status) + Send + 'static>>,
        work_done_cb: Arc<Mutex<dyn FnMut(C2EncodeJob<V>) + Send + 'static>>,
        work_queue: Arc<Mutex<VecDeque<C2EncodeJob<V>>>>,
        state: Arc<Mutex<C2State>>,
        framepool_hint_cb: Arc<Mutex<dyn FnMut(StreamInfo) + Send + 'static>>,
        alloc_cb: Arc<Mutex<dyn FnMut() -> Option<V> + Send + 'static>>,
        options: Self::Options,
    ) -> Result<Self, String> {
        let mut backend = B::new(options)?;
        let (encoder, visible_resolution, coded_resolution) = backend
            .get_encoder(DecodedFormat::from(input_fourcc), EncodedFormat::from(output_fourcc))?;
        (*framepool_hint_cb.lock().unwrap())(StreamInfo {
            format: DecodedFormat::from(input_fourcc),
            coded_resolution: coded_resolution.clone(),
            display_resolution: visible_resolution.clone(),
            // Needs to be equal to the pipeline depth, which is decided by the client. Ideally, we
            // will never need these temp frames though if Gralloc works properly.
            min_num_frames: 0,
        });

        Ok(Self {
            encoder: encoder,
            awaiting_job_event: awaiting_job_event,
            error_cb: error_cb,
            work_done_cb: work_done_cb,
            work_queue: work_queue,
            in_flight_queue: VecDeque::new(),
            state: state,
            alloc_cb: alloc_cb,
            current_tunings: Default::default(),
            visible_resolution: visible_resolution,
            coded_resolution: coded_resolution,
            _phantom: Default::default(),
        })
    }

    fn process_loop(&mut self) {
        let epoll_fd = Epoll::new(EpollCreateFlags::empty()).expect("Failed to create Epoll");
        let _ = epoll_fd
            .add(self.awaiting_job_event.as_fd(), EpollEvent::new(EpollFlags::EPOLLIN, 1))
            .expect("Failed to add job event to Epoll");

        while *self.state.lock().unwrap() == C2State::C2Running {
            let mut events = [EpollEvent::empty()];
            // We need an actual timeout because the encoder is poll based rather than async.
            let _ = epoll_fd
                .wait(&mut events, EpollTimeout::try_from(Duration::from_millis(10)).unwrap())
                .expect("Epoll wait failed");
            if events != [EpollEvent::new(EpollFlags::EPOLLIN, 1)] {
                self.poll_complete_frames();
                continue;
            }

            self.awaiting_job_event.read().unwrap();

            // Unlike the decoder, we can assume a 1:1 relationship between jobs in the work_queue
            // and wake-up events. The encoders are not asynchronous, so the only events we will
            // ever wake-up for are jobs being added to the work_queue.
            let mut job = (*self.work_queue.lock().unwrap())
                .pop_front()
                .expect("Missing job from work queue!");
            if job.get_drain() != DrainMode::NoDrain {
                if let Err(err) = self.encoder.drain() {
                    log::debug!("Error draining encoder! {:?}", err);
                    *self.state.lock().unwrap() = C2State::C2Error;
                    (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                    break;
                }
                if job.get_drain() == DrainMode::EOSDrain {
                    *self.state.lock().unwrap() = C2State::C2Stopped;
                }
            } else {
                let frame_y_stride = job.input.as_ref().unwrap().get_plane_pitch()[0];
                let frame_y_size = job.input.as_ref().unwrap().get_plane_size()[0];
                let can_import_frame = frame_y_stride == self.coded_resolution.width as usize
                    && frame_y_size >= self.coded_resolution.get_area();
                let frame = if can_import_frame {
                    job.input.take().unwrap()
                } else {
                    let mut tmp = (*self.alloc_cb.lock().unwrap())().expect("Allocation failed!");

                    if let Err(_) = convert_video_frame(job.input.as_ref().unwrap(), &mut tmp) {
                        log::debug!("Failed to copy input frame to properly aligned buffer!");
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        break;
                    }
                    {
                        let tmp_mapping = tmp.map_mut().expect("Failed to map tmp frame!");
                        let tmp_planes = tmp_mapping.get();
                        extend_border_nv12(
                            *tmp_planes[Y_PLANE].borrow_mut(),
                            *tmp_planes[UV_PLANE].borrow_mut(),
                            self.visible_resolution.width as usize,
                            self.visible_resolution.height as usize,
                            self.coded_resolution.width as usize,
                            self.coded_resolution.height as usize,
                        );
                    }

                    tmp
                };

                let curr_bitrate = match self.current_tunings.rate_control {
                    ConstantBitrate(bitrate) => bitrate,
                    ConstantQuality(_) => {
                        log::debug!("CQ encoding not currently supported");
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        break;
                    }
                };
                let new_framerate = job.framerate.load(Ordering::Relaxed);
                if job.bitrate != curr_bitrate || new_framerate != self.current_tunings.framerate {
                    self.current_tunings.rate_control = RateControl::ConstantBitrate(job.bitrate);
                    self.current_tunings.framerate = new_framerate;
                    if let Err(err) = self.encoder.tune(self.current_tunings.clone()) {
                        log::debug!("Error adjusting tunings! {:?}", err);
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        break;
                    }
                };

                let meta = FrameMetadata {
                    timestamp: job.timestamp,
                    layout: Default::default(),
                    force_keyframe: false,
                };
                #[cfg(feature = "vaapi")]
                let encode_result = self.encoder.encode(meta, frame);
                #[cfg(feature = "v4l2")]
                let encode_result = self.encoder.encode(meta, V4l2VideoFrame(frame));
                match encode_result {
                    Ok(_) => self.in_flight_queue.push_back(job),
                    Err(err) => {
                        log::debug!("Error encoding frame! {:?}", err);
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        break;
                    }
                }
            }

            self.poll_complete_frames();
        }
    }
}
