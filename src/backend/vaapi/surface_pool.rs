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
use libva::VaError;

use crate::decoder::FramePool;
use crate::Resolution;

/// A VA Surface obtained from a `[SurfacePool]`.
///
/// The surface will automatically be returned to its pool upon dropping, provided the pool still
/// exists and the surface is still compatible with it.
pub struct PooledSurface<M: SurfaceMemoryDescriptor> {
    surface: Option<Surface<M>>,
    pool: Weak<RefCell<SurfacePool<M>>>,
}

impl<M: SurfaceMemoryDescriptor> PooledSurface<M> {
    fn new(surface: Surface<M>, pool: &Rc<RefCell<SurfacePool<M>>>) -> Self {
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
            pool.borrow_mut().managed_surfaces.remove(&surface.id());
        }

        surface
    }
}

impl<M: SurfaceMemoryDescriptor> Borrow<Surface<M>> for PooledSurface<M> {
    fn borrow(&self) -> &Surface<M> {
        // `unwrap` will never fail as `surface` is `Some` until the object is dropped.
        self.surface.as_ref().unwrap()
    }
}

impl<M: SurfaceMemoryDescriptor> AsRef<M> for PooledSurface<M> {
    fn as_ref(&self) -> &M {
        <Self as Borrow<Surface<M>>>::borrow(self).as_ref()
    }
}

impl<M: SurfaceMemoryDescriptor> Drop for PooledSurface<M> {
    fn drop(&mut self) {
        // If the surface has not been detached...
        if let Some(surface) = self.surface.take() {
            // ... and the pool still exists...
            if let Some(pool) = self.pool.upgrade() {
                let mut pool_borrowed = pool.borrow_mut();
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

/// A surface pool to reduce the number of costly Surface allocations.
///
/// The pool only houses Surfaces that fits the pool's coded resolution.
/// Stale surfaces are dropped when either the pool resolution changes, or when
/// stale surfaces are retrieved.
///
/// This means that this pool is suitable for inter-frame DRC, as the stale
/// surfaces will gracefully be dropped, which is arguably better than the
/// alternative of having more than one pool active at a time.
pub struct SurfacePool<M: SurfaceMemoryDescriptor> {
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

impl<M: SurfaceMemoryDescriptor> SurfacePool<M> {
    /// Create a new pool.
    ///
    /// # Arguments
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
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            display,
            rt_format,
            usage_hint,
            coded_resolution,
            surfaces: VecDeque::new(),
            managed_surfaces: Default::default(),
        }))
    }

    /// Create new surfaces and add them to the pool, using `descriptors` as backing memory.
    pub fn add_surfaces(&mut self, descriptors: Vec<M>) -> Result<(), VaError> {
        let surfaces = self.display.create_surfaces(
            self.rt_format,
            // Let the hardware decide the best internal format - we will get the desired fourcc
            // when creating the image.
            None,
            self.coded_resolution.width,
            self.coded_resolution.height,
            self.usage_hint,
            descriptors,
        )?;

        for surface in &surfaces {
            self.managed_surfaces
                .insert(surface.id(), surface.size().into());
        }
        self.surfaces.extend(surfaces);

        Ok(())
    }

    /// Retrieve the current coded resolution of the pool
    pub(crate) fn coded_resolution(&self) -> Resolution {
        self.coded_resolution
    }

    /// Sets the coded resolution of the pool. Releases any stale surfaces.
    pub(crate) fn set_coded_resolution(&mut self, resolution: Resolution) {
        self.coded_resolution = resolution;
        self.managed_surfaces
            .retain(|_, res| res.can_contain(self.coded_resolution));
        self.surfaces
            .retain(|s| Resolution::from(s.size()).can_contain(self.coded_resolution));
    }

    /// Add a surface to the pool.
    ///
    /// This can be an entirely new surface, or one that has been previously obtained using
    /// `get_surface` and is returned.
    ///
    /// Returns an error (and the passed `surface` back) if the surface is not at least as
    /// large as the current coded resolution of the pool.
    #[allow(dead_code)]
    pub(crate) fn add_surface(&mut self, surface: Surface<M>) -> Result<(), Surface<M>> {
        if Resolution::from(surface.size()).can_contain(self.coded_resolution) {
            self.managed_surfaces
                .insert(surface.id(), surface.size().into());
            self.surfaces.push_back(surface);
            Ok(())
        } else {
            Err(surface)
        }
    }

    /// Gets a free surface from the pool.
    ///
    /// `return_pool` is a reference to the smart pointer containing the pool. It is a bit
    /// inelegant, but we unfortunately cannot declare `self` to be `&Rc<RefCell<Self>>` so we
    /// have to use this workaround.
    pub fn get_surface(&mut self, return_pool: &Rc<RefCell<Self>>) -> Option<PooledSurface<M>> {
        let surface = self.surfaces.pop_front();

        // Make sure the invariant holds when debugging. Can save costly
        // debugging time during future refactors, if any.
        debug_assert!({
            match surface.as_ref() {
                Some(s) => Resolution::from(s.size()).can_contain(self.coded_resolution),
                None => true,
            }
        });

        surface.map(|s| PooledSurface::new(s, return_pool))
    }

    /// Returns new number of surfaces left.
    pub(crate) fn num_surfaces_left(&self) -> usize {
        self.surfaces.len()
    }

    /// Returns the total number of managed surfaces in this pool.
    pub(crate) fn num_managed_surfaces(&self) -> usize {
        self.managed_surfaces.len()
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> FramePool<M> for Rc<RefCell<SurfacePool<M>>> {
    fn coded_resolution(&self) -> Resolution {
        (**self).borrow().coded_resolution
    }

    fn set_coded_resolution(&mut self, resolution: Resolution) {
        (**self).borrow_mut().set_coded_resolution(resolution)
    }

    fn add_frames(&mut self, descriptors: Vec<M>) -> Result<(), anyhow::Error> {
        (**self)
            .borrow_mut()
            .add_surfaces(descriptors)
            .map_err(|e| anyhow::anyhow!(e))
    }

    fn num_free_frames(&self) -> usize {
        (**self).borrow().num_surfaces_left()
    }

    fn num_managed_frames(&self) -> usize {
        (**self).borrow().num_managed_surfaces()
    }

    fn clear(&mut self) {
        let mut pool = (**self).borrow_mut();

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
