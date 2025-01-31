// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use nix::errno::Errno;
use nix::sys::eventfd::EfdFlags;
use nix::sys::eventfd::EventFd;

use thiserror::Error;

use std::collections::VecDeque;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::thread::JoinHandle;
use std::vec::Vec;

use crate::decoder::StreamInfo;
use crate::video_frame::VideoFrame;
use crate::Fourcc;
use crate::FrameLayout;
use crate::Resolution;

pub mod c2_decoder;
#[cfg(feature = "v4l2")]
pub mod c2_v4l2_decoder;
#[cfg(feature = "vaapi")]
pub mod c2_vaapi_decoder;

#[derive(Debug)]
pub struct C2DecodeJob<V: VideoFrame> {
    // TODO: Use VideoFrame for input too
    pub input: Vec<u8>,
    pub output: Vec<Arc<V>>,
    pub drain: bool,
    // TODO: Add output delay and color aspect support as needed.
}

impl<V> Job for C2DecodeJob<V>
where
    V: VideoFrame,
{
    type Frame = V;

    fn set_drain(&mut self) {
        self.drain = true;
    }

    fn is_drain(&mut self) -> bool {
        return self.drain;
    }
}

impl<V: VideoFrame> Default for C2DecodeJob<V> {
    fn default() -> Self {
        Self { input: vec![], output: vec![], drain: false }
    }
}

pub trait Job: Send + 'static {
    type Frame: VideoFrame;

    fn set_drain(&mut self) -> ();
    fn is_drain(&mut self) -> bool;
}

#[derive(Debug)]
pub struct C2EncodeJob<V: VideoFrame> {
    pub input: Vec<V>,
    // TODO: Use VideoFrame for output too
    pub output: Vec<u8>,
    // In microseconds.
    pub timestamp: u64,
    // TODO: only support CBR right now, follow up with VBR support.
    pub bitrate: u32,
    // Framerate is actually negotiated, so the encoder can change this value
    // based on the timestamps of the frames it receives.
    pub framerate: Arc<AtomicU32>,

    pub drain: bool,
}

impl<V: VideoFrame> Default for C2EncodeJob<V> {
    fn default() -> Self {
        Self {
            input: vec![],
            output: vec![],
            timestamp: 0,
            bitrate: 0,
            framerate: Arc::new(AtomicU32::new(0)),
            drain: false,
        }
    }
}

impl<V> Job for C2EncodeJob<V>
where
    V: VideoFrame,
{
    type Frame = V;

    fn set_drain(&mut self) {
        self.drain = true;
    }

    fn is_drain(&mut self) -> bool {
        return self.drain;
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum C2State {
    C2Running,
    C2Stopped,
    // Note that on state C2Error, stop() must be called before we can start()
    // again.
    C2Error,
}

// This is not a very "Rust-y" way of doing error handling, but it will
// hopefully make the FFI easier to write. Numerical values taken from
// frameworks/av/media/codec2/core/include/C2.h
// TODO: Improve error handling by adding more statuses.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum C2Status {
    C2Ok = 0,
    C2BadState = 1,  // EPERM
    C2BadValue = 22, // EINVAL
}

// J should be either C2DecodeJob or C2EncodeJob.
pub trait C2Worker<J>
where
    J: Send + Job + 'static,
{
    type Options: Clone + Send + 'static;

    fn new(
        input_fourcc: Fourcc,
        output_fourcc: Fourcc,
        awaiting_job_event: Arc<EventFd>,
        error_cb: Arc<Mutex<dyn FnMut(C2Status) + Send + 'static>>,
        work_done_cb: Arc<Mutex<dyn FnMut(J) + Send + 'static>>,
        work_queue: Arc<Mutex<VecDeque<J>>>,
        state: Arc<Mutex<C2State>>,
        framepool_hint_cb: Arc<Mutex<dyn FnMut(StreamInfo) + Send + 'static>>,
        alloc_cb: Arc<Mutex<dyn FnMut() -> Option<<J as Job>::Frame> + Send + 'static>>,
        options: Self::Options,
    ) -> Result<Self, String>
    where
        Self: Sized;

    fn process_loop(&mut self);
}

#[derive(Debug, Error)]
pub enum C2WrapperError {
    #[error("failed to create EventFd for awaiting job event: {0}")]
    AwaitingJobEventFd(Errno),
}

// Note that we do not guarantee thread safety in C2CrosCodecsWrapper.
pub struct C2Wrapper<J, W>
where
    J: Send + Default + Job + 'static,
    W: C2Worker<J>,
{
    awaiting_job_event: Arc<EventFd>,
    input_fourcc: Fourcc,
    output_fourcc: Fourcc,
    error_cb: Arc<Mutex<dyn FnMut(C2Status) + Send + 'static>>,
    work_done_cb: Arc<Mutex<dyn FnMut(J) + Send + 'static>>,
    work_queue: Arc<Mutex<VecDeque<J>>>,
    state: Arc<Mutex<C2State>>,
    framepool_hint_cb: Arc<Mutex<dyn FnMut(StreamInfo) + Send + 'static>>,
    alloc_cb: Arc<Mutex<dyn FnMut() -> Option<<J as Job>::Frame> + Send + 'static>>,
    options: <W as C2Worker<J>>::Options,
    worker_thread: Option<JoinHandle<()>>,
    // The instance of V actually lives in the thread creation closure, not
    // this struct.
    _phantom: PhantomData<W>,
}

impl<J, W> C2Wrapper<J, W>
where
    J: Send + Default + Job + 'static,
    W: C2Worker<J>,
{
    pub fn new(
        input_fourcc: Fourcc,
        output_fourcc: Fourcc,
        error_cb: impl FnMut(C2Status) + Send + 'static,
        work_done_cb: impl FnMut(J) + Send + 'static,
        framepool_hint_cb: impl FnMut(StreamInfo) + Send + 'static,
        alloc_cb: impl FnMut() -> Option<<J as Job>::Frame> + Send + 'static,
        options: <W as C2Worker<J>>::Options,
    ) -> Self {
        Self {
            awaiting_job_event: Arc::new(
                EventFd::from_flags(EfdFlags::EFD_SEMAPHORE)
                    .map_err(C2WrapperError::AwaitingJobEventFd)
                    .unwrap(),
            ),
            input_fourcc: input_fourcc,
            output_fourcc: output_fourcc,
            error_cb: Arc::new(Mutex::new(error_cb)),
            work_done_cb: Arc::new(Mutex::new(work_done_cb)),
            work_queue: Arc::new(Mutex::new(VecDeque::new())),
            state: Arc::new(Mutex::new(C2State::C2Stopped)),
            framepool_hint_cb: Arc::new(Mutex::new(framepool_hint_cb)),
            alloc_cb: Arc::new(Mutex::new(alloc_cb)),
            options: options,
            worker_thread: None,
            _phantom: Default::default(),
        }
    }

    // This isn't part of C2, but it's convenient to check if our worker thread
    // is still running.
    pub fn is_alive(&self) -> bool {
        match &self.worker_thread {
            Some(worker_thread) => !worker_thread.is_finished(),
            None => false,
        }
    }

    // Start the decoder/encoder.
    // State will be C2Running after this call.
    pub fn start(&mut self) -> C2Status {
        {
            let mut state = self.state.lock().unwrap();
            if *state != C2State::C2Stopped {
                (*self.error_cb.lock().unwrap())(C2Status::C2BadState);
                return C2Status::C2BadState;
            }
            *state = C2State::C2Running;
        }

        let input_fourcc = self.input_fourcc.clone();
        let output_fourcc = self.output_fourcc.clone();
        let error_cb = self.error_cb.clone();
        let work_done_cb = self.work_done_cb.clone();
        let work_queue = self.work_queue.clone();
        let state = self.state.clone();
        let options = self.options.clone();
        let awaiting_job_event = self.awaiting_job_event.clone();
        let framepool_hint_cb = self.framepool_hint_cb.clone();
        let alloc_cb = self.alloc_cb.clone();
        self.worker_thread = Some(thread::spawn(move || {
            let worker = W::new(
                input_fourcc,
                output_fourcc,
                awaiting_job_event,
                error_cb.clone(),
                work_done_cb,
                work_queue,
                state.clone(),
                framepool_hint_cb,
                alloc_cb,
                options,
            );
            match worker {
                Ok(mut worker) => worker.process_loop(),
                Err(msg) => {
                    log::debug!("Error instantiating C2Worker {}", msg);
                    *state.lock().unwrap() = C2State::C2Error;
                    (*error_cb.lock().unwrap())(C2Status::C2BadValue);
                }
            };
        }));

        C2Status::C2Ok
    }

    // Stop the decoder/encoder and abandon in-flight work.
    // C2's reset() function is equivalent for our purposes.
    // Note that in event of error, stop() must be called before we can start()
    // again. This is to ensure we clear out the work queue.
    // State will be C2Stopped after this call.
    pub fn stop(&mut self) -> C2Status {
        *self.state.lock().unwrap() = C2State::C2Stopped;
        let mut worker_thread: Option<JoinHandle<()>> = None;
        std::mem::swap(&mut worker_thread, &mut self.worker_thread);
        self.worker_thread = match worker_thread {
            Some(worker_thread) => {
                let _ = worker_thread.join();
                None
            }
            None => None,
        };

        self.work_queue.lock().unwrap().drain(..);

        self.awaiting_job_event.write(1).unwrap();

        C2Status::C2Ok
    }

    // Add work to the work queue.
    // State must be C2Running or this function is invalid.
    // State will remain C2Running.
    pub fn queue(&mut self, work_items: Vec<J>) -> C2Status {
        if *self.state.lock().unwrap() != C2State::C2Running {
            (*self.error_cb.lock().unwrap())(C2Status::C2BadState);
            return C2Status::C2BadState;
        }

        self.work_queue.lock().unwrap().extend(work_items.into_iter());

        self.awaiting_job_event.write(1).unwrap();

        C2Status::C2Ok
    }

    // Flush work from the queue and return it as |flushed_work|.
    // State will not change after this call.
    // TODO: Support different flush modes.
    pub fn flush(&mut self, flushed_work: &mut Vec<J>) -> C2Status {
        if *self.state.lock().unwrap() != C2State::C2Running {
            (*self.error_cb.lock().unwrap())(C2Status::C2BadState);
            return C2Status::C2BadState;
        }

        let mut tmp = self.work_queue.lock().unwrap().drain(..).collect::<Vec<J>>();
        flushed_work.append(&mut tmp);

        C2Status::C2Ok
    }

    // Signal to the decoder/encoder that it does not need to wait for
    // additional work to begin processing. This is an unusual name for this
    // function, but it is the convention that C2 uses.
    // State must be C2Running or this function is invalid.
    // State will remain C2Running until the last frames drain, at which point
    // the state will change to C2Stopped.
    // TODO: Support different drain modes.
    pub fn drain(&mut self) -> C2Status {
        if *self.state.lock().unwrap() != C2State::C2Running {
            (*self.error_cb.lock().unwrap())(C2Status::C2BadState);
            return C2Status::C2BadState;
        }

        let mut drain_job: J = Default::default();
        drain_job.set_drain();
        self.work_queue.lock().unwrap().push_back(drain_job);

        self.awaiting_job_event.write(1).unwrap();

        C2Status::C2Ok
    }
}

// Instead of C2's release() function, we implement Drop and use RAII to
// accomplish the same thing
impl<J, W> Drop for C2Wrapper<J, W>
where
    J: Send + Default + Job + 'static,
    W: C2Worker<J>,
{
    fn drop(&mut self) {
        self.stop();
    }
}
