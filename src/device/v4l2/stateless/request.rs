// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::fs::File;
use std::os::fd::AsRawFd;
use std::rc::{Rc, Weak};

use v4l2r::controls::ExtControlTrait;
use v4l2r::controls::SafeExtControl;
use v4l2r::ioctl;
use v4l2r::memory::DmaBufHandle;

use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::device::v4l2::stateless::device::V4l2Device;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2OutputBuffer;

struct InitRequestHandle {
    device: V4l2Device,
    timestamp: u64,
    handle: ioctl::Request,
    output_buffer: V4l2OutputBuffer,
    picture: Weak<RefCell<V4l2Picture>>,
}

impl InitRequestHandle {
    fn new(
        device: V4l2Device,
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
    fn submit(self) -> PendingRequestHandle {
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
    fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture>>) {
        self.picture = picture;
    }
}

struct PendingRequestHandle {
    device: V4l2Device,
    timestamp: u64,
    picture: Weak<RefCell<V4l2Picture>>,
}

impl PendingRequestHandle {
    fn sync(self) -> DoneRequestHandle {
        DoneRequestHandle {
            capture_buffer: Rc::new(RefCell::new(self.device.sync(self.timestamp))),
        }
    }
}

struct DoneRequestHandle {
    capture_buffer: Rc<RefCell<V4l2CaptureBuffer<DmaBufHandle<File>>>>,
}

impl DoneRequestHandle {
    fn result(&self) -> V4l2Result {
        V4l2Result { capture_buffer: self.capture_buffer.clone() }
    }
}

#[derive(Default)]
enum RequestHandle {
    Init(InitRequestHandle),
    Pending(PendingRequestHandle),
    Done(DoneRequestHandle),
    #[default]
    Unknown,
}

impl RequestHandle {
    fn new(
        device: V4l2Device,
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
    fn result(&self) -> V4l2Result {
        match self {
            Self::Done(handle) => handle.result(),
            _ => panic!("ERROR"),
        }
    }
    fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture>>) {
        match self {
            Self::Init(handle) => handle.set_picture_ref(picture),
            _ => panic!("ERROR"),
        }
    }
    fn picture(&mut self) -> Weak<RefCell<V4l2Picture>> {
        match self {
            Self::Pending(handle) => handle.picture.clone(),
            _ => panic!("ERROR"),
        }
    }
}

pub struct V4l2Request(RequestHandle);

impl V4l2Request {
    pub fn new(
        device: V4l2Device,
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
    pub fn result(&self) -> V4l2Result {
        self.0.result()
    }
    pub fn set_picture_ref(&mut self, picture: Weak<RefCell<V4l2Picture>>) {
        self.0.set_picture_ref(picture);
    }
    pub fn picture(&mut self) -> Weak<RefCell<V4l2Picture>> {
        self.0.picture()
    }
}

pub struct V4l2Result {
    capture_buffer: Rc<RefCell<V4l2CaptureBuffer<DmaBufHandle<File>>>>,
}

impl V4l2Result {
    pub fn length(&self) -> usize {
        self.capture_buffer.borrow().length()
    }

    pub fn read(&mut self, data: &mut [u8]) {
        self.capture_buffer.borrow_mut().read(data)
    }
}
