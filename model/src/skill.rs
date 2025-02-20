use crate::roll::Roll;

use std::iter::Sum;
use std::num::NonZeroU8;
use std::ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign};

pub const QUALITY_LEVEL_COUNT: usize = 6;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct QualityLevel(NonZeroU8);

impl QualityLevel {

    pub const ONE: QualityLevel = QualityLevel(NonZeroU8::new(1).unwrap());
    pub const TWO: QualityLevel = QualityLevel(NonZeroU8::new(2).unwrap());
    pub const THREE: QualityLevel = QualityLevel(NonZeroU8::new(3).unwrap());
    pub const FOUR: QualityLevel = QualityLevel(NonZeroU8::new(4).unwrap());
    pub const FIVE: QualityLevel = QualityLevel(NonZeroU8::new(5).unwrap());
    pub const SIX: QualityLevel = QualityLevel(NonZeroU8::new(6).unwrap());

    pub const ALL: [QualityLevel; QUALITY_LEVEL_COUNT] = [
        QualityLevel::ONE,
        QualityLevel::TWO,
        QualityLevel::THREE,
        QualityLevel::FOUR,
        QualityLevel::FIVE,
        QualityLevel::SIX
    ];

    fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct QualityLevelMap<T>([T; QUALITY_LEVEL_COUNT]);

impl<T> QualityLevelMap<T> {

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut()
    }
}

impl<T> Index<QualityLevel> for QualityLevelMap<T> {
    type Output = T;

    fn index(&self, index: QualityLevel) -> &T {
        &self.0[index.index()]
    }
}

impl<T> IndexMut<QualityLevel> for QualityLevelMap<T> {
    fn index_mut(&mut self, index: QualityLevel) -> &mut T {
        &mut self.0[index.index()]
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SkillPoints(i32);

impl SkillPoints {

    pub fn new(value: i32) -> SkillPoints {
        SkillPoints(value)
    }

    pub fn quality_level(self) -> Option<QualityLevel> {
        if self.0 < 0 {
            None
        }
        else {
            let quality_level = ((self.0 + 2) / 3).clamp(1, 6);

            Some(QualityLevel(NonZeroU8::new(quality_level as u8).unwrap()))
        }
    }
}

impl AddAssign for SkillPoints {
    fn add_assign(&mut self, rhs: SkillPoints) {
        self.0 += rhs.0;
    }
}

impl Add for SkillPoints {
    type Output = SkillPoints;

    fn add(mut self, rhs: SkillPoints) -> SkillPoints {
        self += rhs;
        self
    }
}

impl Sum<SkillPoints> for SkillPoints {
    fn sum<I: Iterator<Item = SkillPoints>>(iter: I) -> SkillPoints {
        iter.fold(SkillPoints(0), SkillPoints::add)
    }
}

impl SubAssign for SkillPoints {
    fn sub_assign(&mut self, rhs: SkillPoints) {
        self.0 -= rhs.0;
    }
}

impl Sub for SkillPoints {
    type Output = SkillPoints;

    fn sub(mut self, rhs: SkillPoints) -> SkillPoints {
        self -= rhs;
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Attribute(i32);

impl Attribute {

    pub fn new(value: i32) -> Attribute {
        Attribute(value)
    }

    pub fn missing_skill_points(self, roll: Roll) -> SkillPoints {
        SkillPoints((roll.as_u8() as i32 - self.0).max(0))
    }
}
