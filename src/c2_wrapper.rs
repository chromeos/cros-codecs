// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use nix::errno::Errno;
use nix::sys::eventfd::EfdFlags;
use nix::sys::eventfd::EventFd;

use thiserror::Error;

use std::collections::VecDeque;
use std::marker::PhantomData;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::thread::JoinHandle;
use std::vec::Vec;

use crate::Fourcc;
use crate::FrameLayout;

pub mod c2_decoder;
#[cfg(feature = "v4l2")]
pub mod c2_v4l2_decoder;
#[cfg(feature = "vaapi")]
pub mod c2_vaapi_decoder;

pub struct C2VideoFrame<'a> {
    // TODO: This will require us to memcpy frame data. This makes it easy to prototype, but is
    // inefficient. We will want to change this to DMA buffer FDs that we import directly.
    pub planes: Vec<&'a mut [u8]>,
    pub layout: FrameLayout,
}

impl C2VideoFrame<'_> {
    // These frames are coming from an external source, so we should code a
    // a little defensively.
    pub fn validate_frame(
        &self,
        fourcc: Fourcc,
        visible_width: usize,
        visible_height: usize,
    ) -> Result<(), String> {
        if self.planes.len() != self.layout.planes.len() {
            return Err("Malformed FrameLayout!".into());
        }

        if self.layout.format.0 != fourcc {
            return Err("Wrong fourcc!".into());
        }

        match fourcc.to_string().as_str() {
            "I420" => {
                if self.layout.size.width != visible_width as u32
                    || self.layout.size.height != visible_height as u32
                {
                    return Err("Visible dimensions do not match!".into());
                }

                if self.planes.len() != 3 {
                    return Err("Wrong number of planes for I420".into());
                }

                let y_min_size = self.layout.size.get_area();
                let chroma_stride = (self.layout.size.width as usize + 1) / 2;
                let chroma_min_size = chroma_stride * ((self.layout.size.height as usize + 1) / 2);

                if self.layout.planes[0].stride < self.layout.size.width as usize
                    || self.layout.planes[1].stride < chroma_stride
                    || self.layout.planes[2].stride < chroma_stride
                {
                    return Err("Invalid stride!".into());
                }

                if self.planes[0].len() - self.layout.planes[0].offset < y_min_size
                    || self.planes[1].len() - self.layout.planes[1].offset < chroma_min_size
                    || self.planes[2].len() - self.layout.planes[2].offset < chroma_min_size
                {
                    return Err("Insufficient memory allocated!".into());
                }

                Ok(())
            }
            _ => todo!("Format not yet supported!"),
        }
    }
}

pub trait C2DecodeWorkObject {
    fn input(&mut self) -> &[u8];
    // C2 technically supports multiple output worklets, but in practice many decoders don't use
    // this feature. Note that we pass the width and height into this function because we are going
    // to allocate the frame lazily. This is required to support DRC.
    fn output(
        &mut self,
        fourcc: Fourcc,
        visible_width: usize,
        visible_height: usize,
    ) -> Result<C2VideoFrame, String>;
}

#[derive(Debug, Clone, Default)]
pub struct C2DecodeJob<T: C2DecodeWorkObject + Clone + Default> {
    // Many of the callback functions in C2 take C2Work items as parameters, so we need to be able
    // to retrieve the original C++ object.
    pub work_object: T,

    pub drain: bool,
    // TODO: Add output delay and color aspect support as needed.
}

pub trait Job {
    fn set_drain(&mut self) -> ();
    fn is_drain(&mut self) -> bool;
}

pub trait C2EncodeWorkObject {
    fn input(&mut self) -> C2VideoFrame;
    // C2 technically supports multiple output worklets, but in practice many encoders don't use
    // this feature.
    fn output(&mut self) -> Result<&mut [u8], String>;
}

#[derive(Debug, Clone, Default)]
pub struct C2EncodeJob<T: C2EncodeWorkObject + Clone + Default> {
    // Many of the callback functions in C2 take C2Work items as parameters, so
    // we need to be able to retrieve the original C++ object.
    pub work_object: T,
    // In microseconds.
    pub timestamp: u64,
    // TODO: only support CBR right now, follow up with VBR support.
    pub bitrate: u32,
    // Framerate is actually negotiated, so the encoder can change this value
    // based on the timestamps of the frames it receives.
    pub framerate: Arc<AtomicU32>,

    pub drain: bool,
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
pub trait C2Worker<J, E, W>
where
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(J) + Send + 'static,
    J: Send + Job + 'static,
{
    type Options: Clone + Send + 'static;

    fn new(
        fourcc: Fourcc,
        awaiting_job_event: Arc<EventFd>,
        error_cb: Arc<Mutex<E>>,
        work_done_cb: Arc<Mutex<W>>,
        work_queue: Arc<Mutex<VecDeque<J>>>,
        state: Arc<Mutex<C2State>>,
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
pub struct C2Wrapper<J, E, W, V>
where
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(J) + Send + 'static,
    J: Send + Default + Job + 'static,
    V: C2Worker<J, E, W>,
{
    awaiting_job_event: Arc<EventFd>,
    fourcc: Fourcc,
    error_cb: Arc<Mutex<E>>,
    work_done_cb: Arc<Mutex<W>>,
    work_queue: Arc<Mutex<VecDeque<J>>>,
    state: Arc<Mutex<C2State>>,
    options: <V as C2Worker<J, E, W>>::Options,
    worker_thread: Option<JoinHandle<()>>,
    // The instance of V actually lives in the thread creation closure, not
    // this struct.
    _phantom: PhantomData<V>,
}

impl<J, E, W, V> C2Wrapper<J, E, W, V>
where
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(J) + Send + 'static,
    J: Send + Default + Job + 'static,
    V: C2Worker<J, E, W>,
{
    pub fn new(
        fourcc: Fourcc,
        error_cb: E,
        work_done_cb: W,
        options: <V as C2Worker<J, E, W>>::Options,
    ) -> Self {
        Self {
            awaiting_job_event: Arc::new(
                EventFd::from_flags(EfdFlags::EFD_SEMAPHORE)
                    .map_err(C2WrapperError::AwaitingJobEventFd)
                    .unwrap(),
            ),
            fourcc: fourcc,
            error_cb: Arc::new(Mutex::new(error_cb)),
            work_done_cb: Arc::new(Mutex::new(work_done_cb)),
            work_queue: Arc::new(Mutex::new(VecDeque::new())),
            state: Arc::new(Mutex::new(C2State::C2Stopped)),
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

        let fourcc = self.fourcc.clone();
        let error_cb = self.error_cb.clone();
        let work_done_cb = self.work_done_cb.clone();
        let work_queue = self.work_queue.clone();
        let state = self.state.clone();
        let options = self.options.clone();
        let awaiting_job_event = self.awaiting_job_event.clone();
        self.worker_thread = Some(thread::spawn(move || {
            let worker = V::new(
                fourcc,
                awaiting_job_event,
                error_cb.clone(),
                work_done_cb,
                work_queue,
                state.clone(),
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
impl<J, E, W, V> Drop for C2Wrapper<J, E, W, V>
where
    E: FnMut(C2Status) + Send + 'static,
    W: FnMut(J) + Send + 'static,
    J: Send + Default + Job + 'static,
    V: C2Worker<J, E, W>,
{
    fn drop(&mut self) {
        self.stop();
    }
}

impl<T> Job for C2DecodeJob<T>
where
    T: C2DecodeWorkObject + Clone + Default,
{
    fn set_drain(&mut self) {
        self.drain = true;
    }

    fn is_drain(&mut self) -> bool {
        return self.drain;
    }
}

impl<T> Job for C2EncodeJob<T>
where
    T: C2EncodeWorkObject + Clone + Default,
{
    fn set_drain(&mut self) {
        self.drain = true;
    }

    fn is_drain(&mut self) -> bool {
        return self.drain;
    }
}
