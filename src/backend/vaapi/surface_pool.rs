// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::rc::Weak;

use libva::Display;
use libva::Surface;
use libva::SurfaceMemoryDescriptor;
use libva::VASurfaceID;

use crate::decoder::FramePool;
use crate::Resolution;

/// A VA Surface obtained from a `[SurfacePool]`.
///
/// The surface will automatically be returned to its pool upon dropping, provided the pool still
/// exists and the surface is still compatible with it.
pub struct PooledVaSurface<M: SurfaceMemoryDescriptor> {
    surface: Option<Surface<M>>,
    pool: Weak<RefCell<VaSurfacePoolInner<M>>>,
}

impl<M: SurfaceMemoryDescriptor> PooledVaSurface<M> {
    fn new(surface: Surface<M>, pool: &Rc<RefCell<VaSurfacePoolInner<M>>>) -> Self {
        Self {
            surface: Some(surface),
            pool: Rc::downgrade(pool),
        }
    }

    /// Detach this surface from the pool. It will not be returned, and we can dispose of it
    /// freely.
    pub fn detach_from_pool(mut self) -> Surface<M> {
        // `unwrap` will never fail as `surface` is `Some` up to this point.
        let surface = self.surface.take().unwrap();

        if let Some(pool) = self.pool.upgrade() {
            (*pool).borrow_mut().managed_surfaces.remove(&surface.id());
        }

        surface
    }
}

impl<M: SurfaceMemoryDescriptor> Borrow<Surface<M>> for PooledVaSurface<M> {
    fn borrow(&self) -> &Surface<M> {
        // `unwrap` will never fail as `surface` is `Some` until the object is dropped.
        self.surface.as_ref().unwrap()
    }
}

impl<M: SurfaceMemoryDescriptor> AsRef<M> for PooledVaSurface<M> {
    fn as_ref(&self) -> &M {
        <Self as Borrow<Surface<M>>>::borrow(self).as_ref()
    }
}

impl<M: SurfaceMemoryDescriptor> Drop for PooledVaSurface<M> {
    fn drop(&mut self) {
        // If the surface has not been detached...
        if let Some(surface) = self.surface.take() {
            // ... and the pool still exists...
            if let Some(pool) = self.pool.upgrade() {
                let mut pool_borrowed = (*pool).borrow_mut();
                // ... and the pool is still managing this surface, return it.
                if pool_borrowed.managed_surfaces.contains_key(&surface.id()) {
                    pool_borrowed.surfaces.push_back(surface);
                    return;
                }
            }

            // The surface cannot be returned to the pool and can be gracefully dropped.
            log::debug!(
                "Dropping stale surface: {}, ({:?})",
                surface.id(),
                surface.size()
            )
        }
    }
}

struct VaSurfacePoolInner<M: SurfaceMemoryDescriptor> {
    display: Rc<Display>,
    rt_format: u32,
    usage_hint: Option<libva::UsageHint>,
    coded_resolution: Resolution,
    surfaces: VecDeque<Surface<M>>,
    /// All the surfaces managed by this pool, indexed by their surface ID. We keep their
    /// resolution so we can remove them in case of a coded resolution change even if they
    /// are currently borrowed.
    managed_surfaces: BTreeMap<VASurfaceID, Resolution>,
}

/// A surface pool to reduce the number of costly Surface allocations.
///
/// The pool only houses Surfaces that fits the pool's coded resolution.
/// Stale surfaces are dropped when either the pool resolution changes, or when
/// stale surfaces are retrieved.
///
/// This means that this pool is suitable for inter-frame DRC, as the stale
/// surfaces will gracefully be dropped, which is arguably better than the
/// alternative of having more than one pool active at a time.
pub struct VaSurfacePool<M: SurfaceMemoryDescriptor> {
    inner: Rc<RefCell<VaSurfacePoolInner<M>>>,
}

impl<M: SurfaceMemoryDescriptor> VaSurfacePool<M> {
    /// Add a surface to the pool.
    ///
    /// This can be an entirely new surface, or one that has been previously obtained using
    /// `get_surface` and is returned.
    ///
    /// Returns an error (and the passed `surface` back) if the surface is not at least as
    /// large as the current coded resolution of the pool.
    #[allow(dead_code)]
    fn add_surface(&mut self, surface: Surface<M>) -> Result<(), Surface<M>> {
        let mut inner = (*self.inner).borrow_mut();

        if Resolution::from(surface.size()).can_contain(inner.coded_resolution) {
            inner
                .managed_surfaces
                .insert(surface.id(), surface.size().into());
            inner.surfaces.push_back(surface);
            Ok(())
        } else {
            Err(surface)
        }
    }

    /// Create a new pool.
    ///
    // # Arguments
    ///
    /// * `display` - the VA display to create the surfaces from.
    /// * `rt_format` - the VA RT format to use for the surfaces.
    /// * `usage_hint` - hint about how the surfaces from this pool will be used.
    /// * `coded_resolution` - resolution of the surfaces.
    pub fn new(
        display: Rc<Display>,
        rt_format: u32,
        usage_hint: Option<libva::UsageHint>,
        coded_resolution: Resolution,
    ) -> Self {
        Self {
            inner: Rc::new(RefCell::new(VaSurfacePoolInner {
                display,
                rt_format,
                usage_hint,
                coded_resolution,
                surfaces: VecDeque::new(),
                managed_surfaces: Default::default(),
            })),
        }
    }

    /// Gets a free surface from the pool.
    pub fn get_surface(&mut self) -> Option<PooledVaSurface<M>> {
        let mut inner = (*self.inner).borrow_mut();
        let surface = inner.surfaces.pop_front();

        // Make sure the invariant holds when debugging. Can save costly
        // debugging time during future refactors, if any.
        debug_assert!({
            match surface.as_ref() {
                Some(s) => Resolution::from(s.size()).can_contain(inner.coded_resolution),
                None => true,
            }
        });

        surface.map(|s| PooledVaSurface::new(s, &self.inner))
    }
}

impl<M: SurfaceMemoryDescriptor> FramePool<M> for VaSurfacePool<M> {
    fn coded_resolution(&self) -> Resolution {
        (*self.inner).borrow().coded_resolution
    }

    fn set_coded_resolution(&mut self, resolution: Resolution) {
        let mut inner = (*self.inner).borrow_mut();

        inner.coded_resolution = resolution;
        inner
            .managed_surfaces
            .retain(|_, res| res.can_contain(resolution));
        inner
            .surfaces
            .retain(|s| Resolution::from(s.size()).can_contain(resolution));
    }

    fn add_frames(&mut self, descriptors: Vec<M>) -> Result<(), anyhow::Error> {
        let mut inner = (*self.inner).borrow_mut();

        let surfaces = inner
            .display
            .create_surfaces(
                inner.rt_format,
                // Let the hardware decide the best internal format - we will get the desired fourcc
                // when creating the image.
                None,
                inner.coded_resolution.width,
                inner.coded_resolution.height,
                inner.usage_hint,
                descriptors,
            )
            .map_err(|e| anyhow::anyhow!(e))?;

        for surface in &surfaces {
            inner
                .managed_surfaces
                .insert(surface.id(), surface.size().into());
        }
        inner.surfaces.extend(surfaces);

        Ok(())
    }

    fn num_free_frames(&self) -> usize {
        (*self.inner).borrow().surfaces.len()
    }

    fn num_managed_frames(&self) -> usize {
        (*self.inner).borrow().managed_surfaces.len()
    }

    fn clear(&mut self) {
        let mut pool = (*self.inner).borrow_mut();

        pool.surfaces.clear();
        pool.managed_surfaces.clear();
    }

    fn take_free_frame(&mut self) -> Option<Box<dyn AsRef<M>>> {
        (**self)
            .borrow_mut()
            .get_surface(self)
            .map(|s| Box::new(s) as Box<dyn AsRef<M>>)
    }
}
