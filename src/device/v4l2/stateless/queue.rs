// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use v4l2r::bindings::v4l2_format;
use v4l2r::device::queue::direction::Capture;
use v4l2r::device::queue::direction::Output;
use v4l2r::device::queue::dqbuf::DqBuffer;
use v4l2r::device::queue::qbuf::get_free::GetFreeCaptureBuffer;
use v4l2r::device::queue::qbuf::get_free::GetFreeOutputBuffer;
use v4l2r::device::queue::qbuf::weak::QBufferWeak;
use v4l2r::device::queue::BuffersAllocated;
use v4l2r::device::queue::Queue;
use v4l2r::device::queue::QueueInit;
use v4l2r::device::AllocatedQueue;
use v4l2r::device::Device;
use v4l2r::device::Stream;
use v4l2r::device::TryDequeue;
use v4l2r::memory::MemoryType;
use v4l2r::memory::MmapHandle;
use v4l2r::nix::sys::time::TimeVal;
use v4l2r::Format;
use v4l2r::PixelFormat;
use v4l2r::PlaneLayout;

use crate::Resolution;

//TODO: handle memory backends other than mmap
pub struct V4l2OutputBuffer {
    queue: V4l2OutputQueue,
    handle: QBufferWeak<Vec<MmapHandle>, Vec<MmapHandle>>,
    length: usize,
}

impl V4l2OutputBuffer {
    fn new(queue: V4l2OutputQueue, handle: QBufferWeak<Vec<MmapHandle>, Vec<MmapHandle>>) -> Self {
        Self {
            queue,
            handle,
            length: 0,
        }
    }
    pub fn index(&self) -> usize {
        self.handle.index()
    }
    pub fn length(&self) -> usize {
        self.length
    }
    pub fn write(&mut self, data: &[u8]) -> &mut Self {
        let mut mapping = self
            .handle
            .get_plane_mapping(0)
            .expect("Failed to mmap output buffer");

        mapping.as_mut()[self.length..self.length + 3].copy_from_slice(&[0, 0, 1]);
        self.length += 3;

        mapping.as_mut()[self.length..self.length + data.len()].copy_from_slice(data);
        self.length += data.len();

        drop(mapping);
        self
    }
    pub fn submit(self, timestamp: u64, request_fd: i32) {
        let handle = &*self.queue.handle.borrow();
        let queue = match handle {
            V4l2OutputQueueHandle::Streaming(queue) => queue,
            _ => panic!("ERROR"),
        };
        self.handle
            .set_timestamp(TimeVal::new(/* FIXME: sec */ 0, timestamp as i64))
            .set_request(request_fd)
            .queue(&[self.length], queue)
            .expect("Failed to queue output buffer");
    }
}

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
#[derive(Default)]
enum V4l2OutputQueueHandle {
    Init(Queue<Output, QueueInit>),
    Streaming(Queue<Output, BuffersAllocated<Vec<MmapHandle>>>),
    #[default]
    Unknown,
}

#[derive(Clone)]
pub struct V4l2OutputQueue {
    handle: Rc<RefCell<V4l2OutputQueueHandle>>,
    num_buffers: u32,
}

impl V4l2OutputQueue {
    pub fn new(device: Arc<Device>, num_buffers: u32) -> Self {
        let handle = Queue::get_output_mplane_queue(device).expect("Failed to get output queue");
        println!("Output queue:\n\tstate: None -> Init\n");
        let handle = Rc::new(RefCell::new(V4l2OutputQueueHandle::Init(handle)));
        Self {
            handle,
            num_buffers,
        }
    }
    pub fn set_resolution(&mut self, res: Resolution) -> &mut Self {
        self.handle.replace(match self.handle.take() {
            V4l2OutputQueueHandle::Init(mut handle) => {
                let (width, height) = res.into();

                handle.change_format()
                    .expect("Failed to change output format")
                    .set_size(width as usize, height as usize)
                    .set_pixelformat(PixelFormat::from_fourcc(b"S264"))
                    // 1 MB per decoding unit should be enough for most streams.
                    .set_planes_layout(vec![PlaneLayout {
                        sizeimage: 1024 * 1024,
                        ..Default::default()
                    }])
                    .apply::<v4l2_format>()
                    .expect("Failed to apply output format");

                let format: Format = handle.get_format()
                        .expect("Failed to get output format");
                println!(
                    "Output format:\n\t{:?}\n", format
                );

                let handle = handle
                    .request_buffers_generic::<Vec<MmapHandle>>(MemoryType::Mmap, self.num_buffers)
                    .expect("Failed to request output buffers");
                println!(
                    "Output queue:\n\tnum_buffers: {}\n\tnum_queued_buffers: {}\n\tnum_free_buffers: {}\n",
                    handle.num_buffers(), handle.num_queued_buffers(), handle.num_free_buffers()
                );

                // TODO: handle start/stop at runtime
                handle.stream_on()
                    .expect("Failed to start output queue");

                println!("Output queue:\n\tstate: Init -> Streaming\n");
                V4l2OutputQueueHandle::Streaming(handle)
            },
            _ => {
                /* TODO: handle DRC */
                 todo!()
            }
        });
        self
    }
    pub fn num_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => handle.num_buffers(),
            _ => 0,
        }
    }
    pub fn num_free_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => handle.num_free_buffers(),
            _ => 0,
        }
    }
    pub fn alloc_buffer(&self) -> V4l2OutputBuffer {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => V4l2OutputBuffer::new(
                self.clone(),
                handle
                    .try_get_free_buffer()
                    .expect("Failed to alloc output buffer")
                    .take(),
            ),
            _ => panic!("ERROR"),
        }
    }
    pub fn drain(&self) {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => loop {
                match handle.try_dequeue() {
                    Ok(buffer) => continue,
                    _ => break,
                }
            },
            _ => panic!("ERROR"),
        }
    }
}

// TODO: handle other memory backends
pub struct V4l2CaptureBuffer {
    handle: DqBuffer<Capture, Vec<MmapHandle>>,
}

impl V4l2CaptureBuffer {
    fn new(handle: DqBuffer<Capture, Vec<MmapHandle>>) -> Self {
        Self { handle }
    }
    pub fn index(&self) -> usize {
        self.handle.data.index() as usize
    }
    pub fn timestamp(&self) -> u64 {
        self.handle.data.timestamp().tv_usec as u64
    }
    pub fn length(&self) -> usize {
        let mut length = 0;
        for i in 0..self.handle.data.num_planes() {
            let mapping = self
                .handle
                .get_plane_mapping(i)
                .expect("Failed to mmap capture buffer");
            length += mapping.size();
            drop(mapping);
        }
        length
    }
    pub fn read(&self, data: &mut [u8]) {
        let mut offset = 0;
        for i in 0..self.handle.data.num_planes() {
            let mapping = self
                .handle
                .get_plane_mapping(i)
                .expect("Failed to mmap capture buffer");
            data[offset..offset + mapping.size()].copy_from_slice(&mapping);
            offset += mapping.size();
            drop(mapping);
        }
    }
}

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
//TODO: handle synced buffers in Streaming state
#[derive(Default)]
enum V4l2CaptureQueueHandle {
    Init(Queue<Capture, QueueInit>),
    Streaming(Queue<Capture, BuffersAllocated<Vec<MmapHandle>>>),
    #[default]
    Unknown,
}

pub struct V4l2CaptureQueue {
    handle: RefCell<V4l2CaptureQueueHandle>,
    num_buffers: u32,
}

impl V4l2CaptureQueue {
    pub fn new(device: Arc<Device>, num_buffers: u32) -> Self {
        let handle = Queue::get_capture_mplane_queue(device).expect("Failed to get capture queue");
        println!("Capture queue:\n\tstate: None -> Init\n");
        let handle = RefCell::new(V4l2CaptureQueueHandle::Init(handle));
        Self {
            handle,
            num_buffers,
        }
    }
    pub fn set_resolution(&mut self, _: Resolution) -> &mut Self {
        self.handle.replace(match self.handle.take() {
            V4l2CaptureQueueHandle::Init(handle) => {
                let format: Format = handle.get_format()
                        .expect("Failed to get capture format");
                println!(
                    "Capture format:\n\t{:?}\n", format
                );

                let handle = handle
                    .request_buffers_generic::<Vec<MmapHandle>>(MemoryType::Mmap, self.num_buffers)
                    .expect("Failed to request capture buffers");
                println!(
                    "Capture queue:\n\tnum_buffers: {}\n\tnum_queued_buffers: {}\n\tnum_free_buffers: {}\n",
                    handle.num_buffers(), handle.num_queued_buffers(), handle.num_free_buffers()
                );

                // TODO: handle start/stop at runtime
                handle.stream_on()
                    .expect("Failed to start capture queue");

                println!("Capture queue:\n\tstate: Init -> Streaming\n");
                V4l2CaptureQueueHandle::Streaming(handle)
            },
            _ => {
                /* TODO: handle DRC */
                 todo!()
            }
        });
        self
    }
    pub fn dequeue_buffer(&self) -> Option<V4l2CaptureBuffer> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => match handle.try_dequeue() {
                Ok(buffer) => Some(V4l2CaptureBuffer::new(buffer)),
                _ => None,
            },
            _ => panic!("ERROR"),
        }
    }
    pub fn refill(&self) {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => {
                while handle.num_free_buffers() != 0 {
                    handle
                        .try_get_free_buffer()
                        .expect("Failed to alloc capture buffer")
                        .queue()
                        .expect("Failed to queue capture buffer");
                }
            }
            _ => panic!("ERROR"),
        }
    }
}
