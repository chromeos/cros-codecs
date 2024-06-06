// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::os::fd::AsRawFd;
use std::rc::Rc;

use v4l2r::controls::ExtControlTrait;
use v4l2r::controls::SafeExtControl;
use v4l2r::ioctl;

use crate::device::v4l2::stateless::device::V4l2Device;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2OutputBuffer;

struct InitRequestHandle {
    device: V4l2Device,
    timestamp: u64,
    handle: ioctl::Request,
    buffer: V4l2OutputBuffer,
}

impl InitRequestHandle {
    fn new(
        device: V4l2Device,
        timestamp: u64,
        handle: ioctl::Request,
        buffer: V4l2OutputBuffer,
    ) -> Self {
        Self {
            device,
            timestamp,
            handle,
            buffer,
        }
    }
    fn ioctl<C, T>(&mut self, ctrl: C) -> &mut Self
    where
        C: Into<SafeExtControl<T>>,
        T: ExtControlTrait,
    {
        let which = ioctl::CtrlWhich::Request(self.handle.as_raw_fd());
        let mut ctrl: SafeExtControl<T> = ctrl.into();
        ioctl::s_ext_ctrls(&self.device, which, &mut ctrl).expect("Failed to set output control");
        self
    }
    fn write(&mut self, data: &[u8]) -> &mut Self {
        self.buffer.write(data);
        self
    }
    fn submit(self) -> PendingRequestHandle {
        self.buffer.submit(self.timestamp, self.handle.as_raw_fd());
        self.handle.queue().expect("Failed to queue request handle");
        PendingRequestHandle {
            device: self.device.clone(),
            timestamp: self.timestamp,
        }
    }
}

struct PendingRequestHandle {
    device: V4l2Device,
    timestamp: u64,
}

impl PendingRequestHandle {
    fn sync(self) -> DoneRequestHandle {
        DoneRequestHandle {
            buffer: Rc::new(RefCell::new(self.device.sync(self.timestamp))),
        }
    }
}

struct DoneRequestHandle {
    buffer: Rc<RefCell<V4l2CaptureBuffer>>,
}

impl DoneRequestHandle {
    fn result(&self) -> V4l2Result {
        V4l2Result {
            buffer: self.buffer.clone(),
        }
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
        buffer: V4l2OutputBuffer,
    ) -> Self {
        Self::Init(InitRequestHandle::new(device, timestamp, handle, buffer))
    }
    fn timestamp(&self) -> u64 {
        match self {
            Self::Init(handle) => handle.timestamp,
            Self::Pending(handle) => handle.timestamp,
            Self::Done(handle) => handle.buffer.borrow().timestamp(),
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
    fn submit(self) -> Self {
        match self {
            Self::Init(handle) => Self::Pending(handle.submit()),
            _ => panic!("ERROR"),
        }
    }
    fn sync(self) -> Self {
        match self {
            Self::Pending(handle) => Self::Done(handle.sync()),
            Self::Done(_) => self,
            _ => panic!("ERROR"),
        }
    }
    fn result(&self) -> V4l2Result {
        match self {
            Self::Done(handle) => handle.result(),
            _ => panic!("ERROR"),
        }
    }
}

pub struct V4l2Request {
    handle: RefCell<RequestHandle>,
}

impl V4l2Request {
    pub fn new(
        device: V4l2Device,
        timestamp: u64,
        handle: ioctl::Request,
        buffer: V4l2OutputBuffer,
    ) -> Self {
        Self {
            handle: RefCell::new(RequestHandle::new(device, timestamp, handle, buffer)),
        }
    }
    pub fn timestamp(&self) -> u64 {
        self.handle.borrow().timestamp()
    }
    pub fn ioctl<C, T>(&self, ctrl: C) -> &Self
    where
        C: Into<SafeExtControl<T>>,
        T: ExtControlTrait,
    {
        self.handle.borrow_mut().ioctl(ctrl);
        self
    }
    pub fn write(&self, data: &[u8]) -> &Self {
        self.handle.borrow_mut().write(data);
        self
    }
    pub fn submit(&self) -> &Self {
        self.handle.replace(self.handle.take().submit());
        self
    }
    pub fn sync(&self) -> &Self {
        self.handle.replace(self.handle.take().sync());
        self
    }
    pub fn result(&self) -> V4l2Result {
        self.handle.borrow().result()
    }
}

pub struct V4l2Result {
    buffer: Rc<RefCell<V4l2CaptureBuffer>>,
}

impl V4l2Result {
    pub fn length(&self) -> usize {
        self.buffer.borrow().length()
    }
    pub fn read(&self, data: &mut [u8]) {
        self.buffer.borrow().read(data)
    }
}
