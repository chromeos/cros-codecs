// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::os::fd::AsRawFd;
use std::rc::{Rc, Weak};

use v4l2r::controls::ExtControlTrait;
use v4l2r::controls::SafeExtControl;
use v4l2r::ioctl;

use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::device::v4l2::stateless::device::V4l2Device;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2OutputBuffer;
use crate::video_frame::VideoFrame;

struct InitRequestHandle<V: VideoFrame> {
    device: V4l2Device<V>,
    timestamp: u64,
    handle: ioctl::Request,
    output_buffer: V4l2OutputBuffer,
    picture: Weak<RefCell<V4l2Picture<V>>>,
}

impl<V: VideoFrame> InitRequestHandle<V> {
    fn new(
        device: V4l2Device<V>,
        timestamp: u64,
        handle: ioctl::Request,
        output_buffer: V4l2OutputBuffer,
    ) -> Self {
        Self { device, timestamp, handle, output_buffer, picture: Weak::new() }
    }
    fn which(&self) -> ioctl::CtrlWhich {
        ioctl::CtrlWhich::Request(self.handle.as_raw_fd())
    }
    fn ioctl<C, T>(&mut self, ctrl: C) -> &mut Self
    where
        C: Into<SafeExtControl<T>>,
        T: ExtControlTrait,
    {
        let mut ctrl: SafeExtControl<T> = ctrl.into();
        ioctl::s_ext_ctrls(&self.device, self.which(), &mut ctrl)
            .expect("Failed to set output control");
        self
    }
    fn write(&mut self, data: &[u8]) -> &mut Self {
        self.output_buffer.write(data);
        self
    }
    fn submit(self) -> PendingRequestHandle<V> {
        self.output_buffer
            .submit(self.timestamp, self.handle.as_raw_fd())
            .expect("error needs handling");
        self.handle.queue().expect("Failed to queue request handle");
        PendingRequestHandle {
            device: self.device.clone(),
            timestamp: self.timestamp,
            picture: self.picture,
        }
    }
    fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture<V>>>) {
        self.picture = picture;
    }
}

struct PendingRequestHandle<V: VideoFrame> {
    device: V4l2Device<V>,
    timestamp: u64,
    picture: Weak<RefCell<V4l2Picture<V>>>,
}

impl<V: VideoFrame> PendingRequestHandle<V> {
    fn sync(self) -> DoneRequestHandle<V> {
        DoneRequestHandle {
            capture_buffer: Rc::new(RefCell::new(self.device.sync(self.timestamp))),
        }
    }
    fn associate_dequeued_buffer(
        &mut self,
        capture_buffer: V4l2CaptureBuffer<V>,
    ) -> DoneRequestHandle<V> {
        self.picture.upgrade().unwrap().as_ref().try_borrow_mut().unwrap().drop_references();
        DoneRequestHandle { capture_buffer: Rc::new(RefCell::new(capture_buffer)) }
    }
}

struct DoneRequestHandle<V: VideoFrame> {
    capture_buffer: Rc<RefCell<V4l2CaptureBuffer<V>>>,
}

impl<V: VideoFrame> DoneRequestHandle<V> {
    fn result(&self) -> V4l2Result<V> {
        V4l2Result { capture_buffer: self.capture_buffer.clone() }
    }
}

#[derive(Default)]
enum RequestHandle<V: VideoFrame> {
    Init(InitRequestHandle<V>),
    Pending(PendingRequestHandle<V>),
    Done(DoneRequestHandle<V>),
    #[default]
    Unknown,
}

impl<V: VideoFrame> RequestHandle<V> {
    fn new(
        device: V4l2Device<V>,
        timestamp: u64,
        handle: ioctl::Request,
        output_buffer: V4l2OutputBuffer,
    ) -> Self {
        Self::Init(InitRequestHandle::new(device, timestamp, handle, output_buffer))
    }
    fn timestamp(&self) -> u64 {
        match self {
            Self::Init(handle) => handle.timestamp,
            Self::Pending(handle) => handle.timestamp,
            Self::Done(handle) => handle.capture_buffer.borrow().timestamp(),
            _ => panic!("ERROR"),
        }
    }
    fn which(&self) -> ioctl::CtrlWhich {
        match self {
            Self::Init(handle) => handle.which(),
            _ => panic!("ERROR"),
        }
    }
    fn ioctl<C, T>(&mut self, ctrl: C) -> &mut Self
    where
        C: Into<SafeExtControl<T>>,
        T: ExtControlTrait,
    {
        match self {
            Self::Init(handle) => handle.ioctl(ctrl),
            _ => panic!("ERROR"),
        };
        self
    }
    fn write(&mut self, data: &[u8]) -> &mut Self {
        match self {
            Self::Init(handle) => handle.write(data),
            _ => panic!("ERROR"),
        };
        self
    }

    // This method can modify in-place instead of returning a new value. This removes the need for
    // a RefCell in V4l2Request.
    fn submit(&mut self) {
        match std::mem::take(self) {
            Self::Init(handle) => *self = Self::Pending(handle.submit()),
            _ => panic!("ERROR"),
        }
    }
    fn sync(&mut self) {
        match std::mem::take(self) {
            Self::Pending(handle) => *self = Self::Done(handle.sync()),
            s @ Self::Done(_) => *self = s,
            _ => panic!("ERROR"),
        }
    }
    fn result(&self) -> V4l2Result<V> {
        match self {
            Self::Done(handle) => handle.result(),
            _ => panic!("ERROR"),
        }
    }
    fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture<V>>>) {
        match self {
            Self::Init(handle) => handle.set_picture_ref(picture),
            _ => panic!("ERROR"),
        }
    }
    fn picture(&mut self) -> Weak<RefCell<V4l2Picture<V>>> {
        match self {
            Self::Pending(handle) => handle.picture.clone(),
            _ => panic!("ERROR"),
        }
    }
    fn associate_dequeued_buffer(&mut self, capture_buffer: V4l2CaptureBuffer<V>) {
        match self {
            Self::Pending(handle) => {
                *self = Self::Done(handle.associate_dequeued_buffer(capture_buffer))
            }
            _ => panic!("ERROR"),
        }
    }
}

pub struct V4l2Request<V: VideoFrame>(RequestHandle<V>);

impl<V: VideoFrame> V4l2Request<V> {
    pub fn new(
        device: V4l2Device<V>,
        timestamp: u64,
        handle: ioctl::Request,
        output_buffer: V4l2OutputBuffer,
    ) -> Self {
        Self(RequestHandle::new(device, timestamp, handle, output_buffer))
    }
    pub fn timestamp(&self) -> u64 {
        self.0.timestamp()
    }
    pub fn which(&self) -> ioctl::CtrlWhich {
        self.0.which()
    }
    pub fn ioctl<C, T>(&mut self, ctrl: C) -> &mut Self
    where
        C: Into<SafeExtControl<T>>,
        T: ExtControlTrait,
    {
        self.0.ioctl(ctrl);
        self
    }
    pub fn write(&mut self, data: &[u8]) -> &mut Self {
        self.0.write(data);
        self
    }
    pub fn submit(&mut self) {
        self.0.submit();
    }
    pub fn sync(&mut self) {
        self.0.sync();
    }
    pub fn result(&self) -> V4l2Result<V> {
        self.0.result()
    }
    pub fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture<V>>>) {
        self.0.set_picture_ref(picture);
    }
    pub fn picture(&mut self) -> Weak<RefCell<V4l2Picture<V>>> {
        self.0.picture()
    }
    pub fn associate_dequeued_buffer(&mut self, capture_buffer: V4l2CaptureBuffer<V>) {
        self.0.associate_dequeued_buffer(capture_buffer)
    }
}

pub struct V4l2Result<V: VideoFrame> {
    pub capture_buffer: Rc<RefCell<V4l2CaptureBuffer<V>>>,
}
