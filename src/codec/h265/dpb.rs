// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::rc::Rc;

use anyhow::anyhow;

use crate::codec::h265::parser::Sps;
use crate::codec::h265::picture::PictureData;
use crate::codec::h265::picture::Reference;

// Shortcut to refer to a DPB entry.
//
// The first member of the tuple is the `PictureData` for the frame.
//
// The second member is the backend handle of the frame.
#[derive(Clone, Debug)]
pub struct DpbEntry<T>(pub Rc<RefCell<PictureData>>, pub T);

pub struct Dpb<T> {
    /// List of `PictureData` and backend handles to decoded pictures.
    entries: Vec<DpbEntry<T>>,
    /// The maximum number of pictures that can be stored.
    max_num_pics: usize,
}

impl<T: Clone> Dpb<T> {
    /// Returns an iterator over the underlying H265 pictures stored in the
    /// DPB.
    pub fn pictures(&self) -> impl Iterator<Item = Ref<'_, PictureData>> {
        self.entries.iter().map(|h| h.0.borrow())
    }

    /// Returns a mutable iterator over the underlying H265 pictures stored in
    /// the DPB.
    pub fn pictures_mut(&mut self) -> impl Iterator<Item = RefMut<'_, PictureData>> {
        self.entries.iter().map(|h| h.0.borrow_mut())
    }

    /// Returns the length of the DPB.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get a reference to the whole DPB entries.
    pub fn entries(&self) -> &Vec<DpbEntry<T>> {
        &self.entries
    }

    /// Set the dpb's max num pics.
    pub fn set_max_num_pics(&mut self, max_num_pics: usize) {
        self.max_num_pics = max_num_pics;
    }

    /// Get a reference to the dpb's max num pics.
    pub fn max_num_pics(&self) -> usize {
        self.max_num_pics
    }

    /// Mark all pictures in the DPB as unused for reference.
    pub fn mark_all_as_unused_for_ref(&mut self) {
        for mut picture in self.pictures_mut() {
            picture.reference = Reference::None;
        }
    }

    /// Gets the position of `needle` in the DPB, if any.
    fn get_position(&self, needle: &Rc<RefCell<PictureData>>) -> Option<usize> {
        self.entries
            .iter()
            .position(|handle| Rc::ptr_eq(&handle.0, needle))
    }

    /// Finds a reference picture in the DPB using `poc`.
    pub fn find_ref_by_poc(&self, poc: i32) -> Option<DpbEntry<T>> {
        let position = self
            .pictures()
            .position(|p| p.is_ref() && p.pic_order_cnt_val == poc);

        log::debug!("find_ref_by_poc: {}, found position {:?}", poc, position);
        Some(self.entries[position?].clone())
    }

    /// Finds a short term reference picture in the DPB using `poc`.
    pub fn find_short_term_ref_by_poc(&self, poc: i32) -> Option<DpbEntry<T>> {
        let position = self.pictures().position(|p| {
            matches!(p.reference, Reference::ShortTerm) && p.pic_order_cnt_val == poc
        });

        log::debug!(
            "find_short_term_ref_by_poc: {}, found position {:?}",
            poc,
            position
        );
        Some(self.entries[position?].clone())
    }

    /// Drains the DPB by continuously invoking the bumping process.
    pub fn drain(&mut self) -> Vec<DpbEntry<T>> {
        log::debug!("Draining the DPB.");

        let mut pics = vec![];
        while let Some(pic) = self.bump(true) {
            pics.push(pic);
        }

        pics
    }

    /// Whether the DPB needs bumping. See C.5.2.2.
    pub fn needs_bumping(&mut self, sps: &Sps) -> bool {
        let num_needed_for_output = self.pictures().filter(|pic| pic.needed_for_output).count();

        let highest_tid = sps.max_sub_layers_minus1();
        let max_num_reorder_pics = sps.max_num_reorder_pics()[usize::from(highest_tid)];
        let max_latency_increase_plus1 = sps.max_latency_increase_plus1()[usize::from(highest_tid)];
        let pic_over_max_latency = self.pictures().find(|pic| {
            pic.needed_for_output && pic.pic_latency_cnt >= i32::from(max_latency_increase_plus1)
        });
        let max_dec_pic_buffering =
            usize::from(sps.max_dec_pic_buffering_minus1()[usize::from(highest_tid)]) + 1;

        num_needed_for_output > max_num_reorder_pics.into()
            || (max_latency_increase_plus1 != 0 && pic_over_max_latency.is_some())
            || self.entries().len() > max_dec_pic_buffering
    }

    /// Find the lowest POC in the DPB that can be bumped.
    fn find_lowest_poc_for_bumping(&self) -> Option<DpbEntry<T>> {
        let lowest = self
            .pictures()
            .filter(|pic| pic.needed_for_output)
            .min_by_key(|pic| pic.pic_order_cnt_val)?;

        let position = self
            .entries
            .iter()
            .position(|handle| handle.0.borrow().pic_order_cnt_val == lowest.pic_order_cnt_val)
            .unwrap();

        Some(self.entries[position].clone())
    }

    /// See C.5.2.4 "Bumping process".
    pub fn bump(&mut self, flush: bool) -> Option<DpbEntry<T>> {
        let handle = self.find_lowest_poc_for_bumping()?;
        let mut pic = handle.0.borrow_mut();

        pic.needed_for_output = false;
        log::debug!("Bumping picture {:#?} from the dpb", pic);

        if !pic.is_ref() || flush {
            let index = self.get_position(&handle.0).unwrap();
            log::debug!("removed picture {:#?} from dpb", pic);
            self.entries.remove(index);
        }

        Some(handle.clone())
    }

    /// See C.5.2.3. Happens when we are done decoding the picture.
    pub fn needs_additional_bumping(&mut self, sps: &Sps) -> bool {
        let num_needed_for_output = self.pictures().filter(|pic| pic.needed_for_output).count();
        let highest_tid = sps.max_sub_layers_minus1();

        let max_num_reorder_pics = sps.max_num_reorder_pics()[usize::from(highest_tid)];
        let max_latency_increase_plus1 = sps.max_latency_increase_plus1()[usize::from(highest_tid)];

        let pic_over_max_latency = self.pictures().find(|pic| {
            pic.needed_for_output && pic.pic_latency_cnt >= i32::from(max_latency_increase_plus1)
        });

        num_needed_for_output > max_num_reorder_pics.into()
            || (max_latency_increase_plus1 != 0 && pic_over_max_latency.is_some())
    }

    /// Clears the DPB, dropping all the pictures.
    pub fn clear(&mut self) {
        log::debug!("Clearing the DPB");

        let max_num_pics = self.max_num_pics;

        *self = Default::default();
        self.max_num_pics = max_num_pics;
    }

    /// Removes all pictures which are marked as "not needed for output" and
    /// "unused for reference". See C.5.2.2
    pub fn remove_unused(&mut self) {
        log::debug!("Removing unused pictures from DPB.");
        self.entries.retain(|e| {
            let pic = e.0.borrow();
            let retain = pic.needed_for_output || pic.is_ref();
            log::debug!("Retaining pic POC: {}: {}", pic.pic_order_cnt_val, retain);
            retain
        })
    }

    /// Store a picture and its backend handle in the DPB.
    pub fn store_picture(
        &mut self,
        picture: Rc<RefCell<PictureData>>,
        handle: T,
    ) -> anyhow::Result<()> {
        if self.entries.len() >= self.max_num_pics {
            return Err(anyhow!("Can't add a picture to the DPB: DPB is full."));
        }

        let mut pic = picture.borrow_mut();
        log::debug!(
            "Stored picture POC {:?}, the DPB length is {:?}",
            pic.pic_order_cnt_val,
            self.entries.len()
        );

        if pic.pic_output_flag {
            pic.needed_for_output = true;
            pic.pic_latency_cnt = 0;
        } else {
            pic.needed_for_output = false;
        }

        // C.3.4.
        // After all the slices of the current picture have been decoded, this
        // picture is marked as "used for short-term reference".
        pic.reference = Reference::ShortTerm;
        drop(pic);

        for mut pic in self.pictures_mut() {
            pic.pic_latency_cnt += 1;
        }

        self.entries.push(DpbEntry(picture, handle));

        Ok(())
    }

    /// Returns all the references in the DPB.
    pub fn get_all_references(&self) -> Vec<DpbEntry<T>> {
        self.entries
            .iter()
            .filter(|e| e.0.borrow().is_ref())
            .cloned()
            .collect()
    }
}

impl<T: Clone> Default for Dpb<T> {
    fn default() -> Self {
        // See https://github.com/rust-lang/rust/issues/26925 on why this can't
        // be derived.
        Self {
            entries: Default::default(),
            max_num_pics: Default::default(),
        }
    }
}

impl<T: Clone> std::fmt::Debug for Dpb<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pics = self
            .entries
            .iter()
            .map(|h| &h.0)
            .enumerate()
            .collect::<Vec<_>>();
        f.debug_struct("Dpb")
            .field("pictures", &pics)
            .field("max_num_pics", &self.max_num_pics)
            .finish()
    }
}
