// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::clone::Clone;
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use crate::c2_wrapper::C2DecodeJob;
use crate::c2_wrapper::C2DecodeWorkObject;
use crate::c2_wrapper::C2State;
use crate::c2_wrapper::C2Status;
use crate::c2_wrapper::C2Worker;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::FramePool;
use crate::decoder::StreamInfo;
use crate::image_processing::i4xx_copy;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;

pub trait C2DecoderBackend<D, H, FP, U>
where
    H: DecodedHandle,
    FP: FramePool<Descriptor = H::Descriptor> + ?Sized,
    D: StatelessVideoDecoder<Handle = H, FramePool = FP> + ?Sized,
{
    fn new(options: U) -> Result<Self, String>
    where
        Self: Sized;
    fn get_decoder(&mut self, format: EncodedFormat) -> Result<Box<D>, String>;
    fn allocate_new_frames(
        &mut self,
        stream_info: &StreamInfo,
        num_frames: usize,
    ) -> Result<Vec<H::Descriptor>, String>;
}

pub struct C2DecoderWorker<T, B, D, H, FP, U, E, W>
where
    T: C2DecodeWorkObject + Clone + Send + 'static,
    H: DecodedHandle,
    FP: FramePool<Descriptor = H::Descriptor> + ?Sized,
    D: StatelessVideoDecoder<Handle = H, FramePool = FP> + ?Sized,
    B: C2DecoderBackend<D, H, FP, U>,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
    U: Clone + Send + 'static,
{
    backend: B,
    decoder: Box<D>,
    error_cb: Arc<Mutex<E>>,
    work_done_cb: Arc<Mutex<W>>,
    work_queue: Arc<Mutex<VecDeque<C2DecodeJob<T>>>>,
    pending_job: Option<C2DecodeJob<T>>,
    in_flight_queue: VecDeque<C2DecodeJob<T>>,
    frame_num: u64,
    state: Arc<Mutex<C2State>>,
    // TODO: Is this really the only way to fix the unconstrained type problem?
    _phantom: PhantomData<(H, U)>,
    // Tuples can't have more than one un-sized type, so we have to use this
    // workaround.
    __phantom: PhantomData<H>,
}

impl<T, B, D, H, FP, U, E, W> C2DecoderWorker<T, B, D, H, FP, U, E, W>
where
    T: C2DecodeWorkObject + Clone + Send + 'static,
    B: C2DecoderBackend<D, H, FP, U>,
    H: DecodedHandle,
    FP: FramePool<Descriptor = H::Descriptor> + ?Sized,
    D: StatelessVideoDecoder<Handle = H, FramePool = FP> + ?Sized,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
    U: Clone + Send + 'static,
{
    // Returns true if there were any events to process
    fn check_events(&mut self) -> bool {
        let mut ret = false;
        while let Some(event) = self.decoder.next_event() {
            ret = true;
            match event {
                DecoderEvent::FrameReady(frame) => {
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
                    let picture = frame.dyn_picture();
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
                    format_setter.try_format(DecodedFormat::I420).unwrap();
                    let stream_info = format_setter.stream_info().clone();
                    let min_num_frames = stream_info.min_num_frames;
                    let pools = format_setter.frame_pool(PoolLayer::All);
                    let num_pools = pools.len();
                    for pool in pools {
                        let pool_num_frames = pool.num_managed_frames();
                        if pool_num_frames < (min_num_frames / num_pools) {
                            let frames = match self
                                .backend
                                .allocate_new_frames(&stream_info, min_num_frames - pool_num_frames)
                            {
                                Ok(frames) => frames,
                                Err(msg) => {
                                    log::debug!("Failed to allocate pool frames! {}", msg);
                                    *self.state.lock().unwrap() = C2State::C2Error;
                                    (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                                    return true;
                                }
                            };
                            pool.add_frames(frames).unwrap();
                        }
                    }
                }
            };
        }

        ret
    }
}

impl<T, B, D, H, FP, U, E, W> C2Worker<C2DecodeJob<T>, U, E, W>
    for C2DecoderWorker<T, B, D, H, FP, U, E, W>
where
    T: C2DecodeWorkObject + Clone + Send + 'static,
    B: C2DecoderBackend<D, H, FP, U>,
    H: DecodedHandle,
    FP: FramePool<Descriptor = H::Descriptor> + ?Sized,
    D: StatelessVideoDecoder<Handle = H, FramePool = FP> + ?Sized,
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(C2DecodeJob<T>) + Send + 'static,
    U: Clone + Send + 'static,
{
    fn new(
        fourcc: Fourcc,
        error_cb: Arc<Mutex<E>>,
        work_done_cb: Arc<Mutex<W>>,
        work_queue: Arc<Mutex<VecDeque<C2DecodeJob<T>>>>,
        state: Arc<Mutex<C2State>>,
        options: U,
    ) -> Result<Self, String> {
        let mut backend = B::new(options)?;
        let decoder = backend.get_decoder(EncodedFormat::from(fourcc))?;
        Ok(Self {
            backend: backend,
            decoder: decoder,
            error_cb: error_cb,
            work_done_cb: work_done_cb,
            work_queue: work_queue,
            pending_job: None,
            in_flight_queue: VecDeque::new(),
            frame_num: 0,
            state: state,
            _phantom: Default::default(),
            __phantom: Default::default(),
        })
    }

    fn process_loop(&mut self) {
        while *self.state.lock().unwrap() == C2State::C2Running {
            let mut did_nothing = true;

            let mut pending_job: Option<C2DecodeJob<T>> = None;
            std::mem::swap(&mut self.pending_job, &mut pending_job);
            self.pending_job = match pending_job {
                Some(mut job) => {
                    let bitstream = job.work_object.input();
                    match self.decoder.decode(self.frame_num, bitstream) {
                        Ok(_) => {
                            did_nothing = false;
                            self.frame_num += 1;
                            self.in_flight_queue.push_back(job);
                            None
                        }
                        Err(DecodeError::CheckEvents)
                        | Err(DecodeError::NotEnoughOutputBuffers(_)) => Some(job),
                        Err(e) => {
                            did_nothing = false;
                            log::debug!("Unhandled error message from decoder {e:?}");
                            *self.state.lock().unwrap() = C2State::C2Error;
                            (*self.error_cb.lock().unwrap())(C2Status::C2BadValue);
                            None
                        }
                    }
                }
                None => {
                    let ret = (*self.work_queue.lock().unwrap()).pop_front();
                    if ret.is_some() {
                        did_nothing = false;
                    }
                    ret
                }
            };

            did_nothing = !self.check_events() && did_nothing;

            if did_nothing {
                //TODO: Tune this value.
                //TODO: Maybe we should come up with a way to make this async instead of polling?
                thread::sleep(Duration::from_millis(10));
            }
        }
    }
}
