// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use nix::errno::Errno;
use nix::sys::epoll::Epoll;
use nix::sys::epoll::EpollCreateFlags;
use nix::sys::epoll::EpollEvent;
use nix::sys::epoll::EpollFlags;
use nix::sys::epoll::EpollTimeout;
use nix::sys::eventfd::EventFd;

use std::os::fd::AsFd;
use thiserror::Error;

use std::clone::Clone;
use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::Mutex;

use crate::c2_wrapper::C2DecodeJob;
use crate::c2_wrapper::C2DecodeWorkObject;
use crate::c2_wrapper::C2State;
use crate::c2_wrapper::C2Status;
use crate::c2_wrapper::C2Worker;
use crate::c2_wrapper::Job;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DynStatelessVideoDecoder;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::StreamInfo;
use crate::image_processing::i4xx_copy;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;

#[derive(Debug, Error)]
pub enum C2DecoderPollErrorWrapper {
    #[error("failed to create Epoll: {0}")]
    Epoll(Errno),
    #[error("failed to add poll FDs to Epoll: {0}")]
    EpollAdd(Errno),
}

pub trait C2DecoderBackend {
    type DecodedHandle;
    type DecoderOptions: Clone + Send + 'static;

    fn new(options: Self::DecoderOptions) -> Result<Self, String>
    where
        Self: Sized;
    // TODO: Support stateful video decoders.
    fn get_decoder(
        &mut self,
        format: EncodedFormat,
    ) -> Result<DynStatelessVideoDecoder<Self::DecodedHandle>, String>;
}

pub struct C2DecoderWorker<B, T, E, W>
where
    B: C2DecoderBackend,
    T: C2DecodeWorkObject + Clone + Default + Send + 'static,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
{
    backend: B,
    decoder: DynStatelessVideoDecoder<<B as C2DecoderBackend>::DecodedHandle>,
    epoll_fd: Epoll,
    awaiting_job_event: Arc<EventFd>,
    error_cb: Arc<Mutex<E>>,
    work_done_cb: Arc<Mutex<W>>,
    work_queue: Arc<Mutex<VecDeque<C2DecodeJob<T>>>>,
    pending_job: Option<C2DecodeJob<T>>,
    in_flight_queue: VecDeque<C2DecodeJob<T>>,
    frame_num: u64,
    state: Arc<Mutex<C2State>>,
}

impl<B, T, E, W> C2DecoderWorker<B, T, E, W>
where
    B: C2DecoderBackend,
    T: C2DecodeWorkObject + Clone + Default + Send + 'static,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
{
    // Returns true if there were any events to process
    fn check_events(&mut self) -> bool {
        let mut ret = false;
        while let Some(event) = self.decoder.next_event() {
            ret = true;
            match event {
                DecoderEvent::FrameReady(mut frame) => {
                    frame.sync().unwrap();

                    // TODO: This should never fail if the queue logic is
                    // correct, but maybe we should be more robust here
                    let mut job = self.in_flight_queue.pop_front().unwrap();

                    let visible_resolution = frame.display_resolution();
                    let work_object = &mut job.work_object;
                    let mut output_frame = match work_object.output(
                        Fourcc::from(b"I420"),
                        visible_resolution.width as usize,
                        visible_resolution.height as usize,
                    ) {
                        Ok(output_frame) => output_frame,
                        Err(msg) => {
                            log::debug!("Output frame failed to allocate! {msg}");
                            *self.state.lock().unwrap() = C2State::C2Error;
                            (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                            return ret;
                        }
                    };
                    if let Err(msg) = &output_frame.validate_frame(
                        Fourcc::from(b"I420"),
                        visible_resolution.width as usize,
                        visible_resolution.height as usize,
                    ) {
                        log::debug!("Output frame invalid! {msg}");
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        return ret;
                    }

                    // TODO: This is several unnecessary memcpy's. We're going to need to rework
                    // everything about frame management in order to achieve zero copy.
                    let mut picture = frame.dyn_picture();
                    let mut handle = picture.dyn_mappable_handle().unwrap();
                    let buffer_size = handle.image_size();
                    let mut tmp_buffer = vec![0; buffer_size];
                    if let Err(msg) = handle.read(&mut tmp_buffer) {
                        log::debug!("Error reading data from mapped handle! {}", msg);
                        *self.state.lock().unwrap() = C2State::C2Error;
                        (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        return ret;
                    }

                    // MappableHandle returns a frame with all planes in the same buffer and no
                    // padding, so we can just assume stride = width.
                    let (src_y, src_uv) = tmp_buffer
                        .split_at((visible_resolution.width * visible_resolution.height) as usize);
                    let (src_u, src_v) = src_uv.split_at(
                        (((visible_resolution.width + 1) / 2)
                            * ((visible_resolution.height + 1) / 2))
                            as usize,
                    );
                    let (dst_y, dst_uv) = output_frame.planes.split_at_mut(1);
                    let (dst_u, dst_v) = dst_uv.split_at_mut(1);
                    i4xx_copy(
                        src_y,
                        visible_resolution.width as usize,
                        dst_y[0],
                        output_frame.layout.planes[0].stride,
                        src_u,
                        (visible_resolution.width as usize + 1) / 2,
                        dst_u[0],
                        output_frame.layout.planes[1].stride,
                        src_v,
                        (visible_resolution.width as usize + 1) / 2,
                        dst_v[0],
                        output_frame.layout.planes[2].stride,
                        visible_resolution.width as usize,
                        visible_resolution.height as usize,
                        (true, true),
                    );

                    (*self.work_done_cb.lock().unwrap())(job);
                }
                DecoderEvent::FormatChanged(mut format_setter) => {
                    //TODO: Support more formats. Also, don't panic.
                    //TODO: Re-allocate Gralloc frame pool.
                    format_setter.try_format(DecodedFormat::I420).unwrap();
                }
            };
        }

        ret
    }
}

impl<B, T, E, W> C2Worker<C2DecodeJob<T>, E, W> for C2DecoderWorker<B, T, E, W>
where
    B: C2DecoderBackend,
    T: C2DecodeWorkObject + Clone + Default + Send + 'static,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
{
    type Options = <B as C2DecoderBackend>::DecoderOptions;

    fn new(
        fourcc: Fourcc,
        awaiting_job_event: Arc<EventFd>,
        error_cb: Arc<Mutex<E>>,
        work_done_cb: Arc<Mutex<W>>,
        work_queue: Arc<Mutex<VecDeque<C2DecodeJob<T>>>>,
        state: Arc<Mutex<C2State>>,
        options: Self::Options,
    ) -> Result<Self, String> {
        let mut backend = B::new(options)?;
        let decoder = backend.get_decoder(EncodedFormat::from(fourcc))?;
        Ok(Self {
            backend: backend,
            decoder: decoder,
            epoll_fd: Epoll::new(EpollCreateFlags::empty())
                .map_err(C2DecoderPollErrorWrapper::Epoll)
                .unwrap(),
            awaiting_job_event: awaiting_job_event,
            error_cb: error_cb,
            work_done_cb: work_done_cb,
            work_queue: work_queue,
            pending_job: None,
            in_flight_queue: VecDeque::new(),
            frame_num: 0,
            state: state,
        })
    }

    fn process_loop(&mut self) {
        self.epoll_fd
            .add(self.decoder.poll_fd(), EpollEvent::new(EpollFlags::EPOLLIN, 1))
            .map_err(C2DecoderPollErrorWrapper::EpollAdd)
            .unwrap();
        self.epoll_fd
            .add(self.awaiting_job_event.as_fd(), EpollEvent::new(EpollFlags::EPOLLIN, 2))
            .map_err(C2DecoderPollErrorWrapper::EpollAdd)
            .unwrap();

        while *self.state.lock().unwrap() == C2State::C2Running {
            // Poll for decoder events or pending job events.
            let mut events = [EpollEvent::empty()];
            self.epoll_fd.wait(&mut events, EpollTimeout::ZERO).unwrap();

            if events == [EpollEvent::new(EpollFlags::EPOLLIN, 1)] {
                self.awaiting_job_event.read().unwrap();
            } else if events == [EpollEvent::new(EpollFlags::EPOLLIN, 2)] {
                self.decoder.next_event();
            }

            let mut pending_job: Option<C2DecodeJob<T>> = None;
            std::mem::swap(&mut self.pending_job, &mut pending_job);
            self.pending_job = match pending_job {
                Some(mut job) => {
                    if job.is_drain() {
                        if let Err(_) = self.decoder.flush() {
                            log::debug!("Error handling drain request!");
                            *self.state.lock().unwrap() = C2State::C2Error;
                            (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                        } else {
                            *self.state.lock().unwrap() = C2State::C2Stopped;
                        }
                        None
                    } else {
                        let bitstream = job.work_object.input();
                        match self.decoder.decode(self.frame_num, bitstream) {
                            Ok(_) => {
                                self.frame_num += 1;
                                self.in_flight_queue.push_back(job);
                                None
                            }
                            Err(DecodeError::CheckEvents)
                            | Err(DecodeError::NotEnoughOutputBuffers(_)) => Some(job),
                            Err(e) => {
                                log::debug!("Unhandled error message from decoder {e:?}");
                                *self.state.lock().unwrap() = C2State::C2Error;
                                (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                                None
                            }
                        }
                    }
                }
                None => {
                    let ret = (*self.work_queue.lock().unwrap()).pop_front();
                    ret
                }
            };

            self.check_events();
        }
    }
}
