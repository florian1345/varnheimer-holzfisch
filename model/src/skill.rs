use crate::roll::Roll;

use std::iter::Sum;
use std::num::NonZeroU8;
use std::ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign};
use std::slice::Iter;

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

    pub fn saturating_add(self, other: QualityLevel) -> QualityLevel {
        QualityLevel(self.0.checked_add(other.0.get()).unwrap().min(QualityLevel::SIX.0))
    }

    pub fn saturating_add_option(self, other: Option<QualityLevel>) -> QualityLevel {
        other
            .map(|other_ql| self.saturating_add(other_ql))
            .unwrap_or(self)
    }

    pub fn as_u8(self) -> u8 {
        self.0.get()
    }

    fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct QualityLevelMap<T>([T; QUALITY_LEVEL_COUNT]);

impl<T> QualityLevelMap<T> {

    pub fn iter(&self) -> Iter<'_, T> {
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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::test_util;

    use kernal::collections::Collection;
    use kernal::collections::ordered::OrderedCollection;
    use kernal::prelude::*;

    use rstest::rstest;

    impl<'collection, T> Collection<'collection> for QualityLevelMap<T> {
        type Item = T;
        type Iter<'iter> = Iter<'iter, T>
        where
            Self: 'iter,
            'collection: 'iter;

        fn iterator<'reference>(&'reference self) -> Self::Iter<'reference>
        where
            'collection: 'reference
        {
            self.iter()
        }
    }
    
    impl<'collection, T> OrderedCollection<'collection> for QualityLevelMap<T> {}

    #[test]
    fn all_quality_levels_correctly_initialized() {
        let all_quality_levels_as_u8 = QualityLevel::ALL.iter()
            .map(|ql| ql.0.get())
            .collect::<Vec<_>>();

        assert_that!(all_quality_levels_as_u8).contains_exactly_in_given_order(1..=6);
    }

    #[test]
    fn quality_level_map_indexing_works() {
        let map = test_util::create_quality_level_map([5, 10, 15, 20, 25, 30]);

        assert_that!(map[QualityLevel::ONE]).is_equal_to(5);
        assert_that!(map[QualityLevel::TWO]).is_equal_to(10);
        assert_that!(map[QualityLevel::THREE]).is_equal_to(15);
        assert_that!(map[QualityLevel::FOUR]).is_equal_to(20);
        assert_that!(map[QualityLevel::FIVE]).is_equal_to(25);
        assert_that!(map[QualityLevel::SIX]).is_equal_to(30);
    }

    #[test]
    fn quality_level_iter_works() {
        let map = test_util::create_quality_level_map([5, 10, 15, 20, 25, 30]);
        let iter_collected = map.iter().cloned().collect::<Vec<_>>();
        let iter_mut_collected = map.iter().cloned().collect::<Vec<_>>();

        assert_that!(iter_collected).contains_exactly_in_given_order([5, 10, 15, 20, 25, 30]);
        assert_that!(iter_mut_collected).contains_exactly_in_given_order([5, 10, 15, 20, 25, 30]);
    }

    #[rstest]
    #[case(-1)]
    #[case(-10)]
    #[case(i32::MIN)]
    fn skill_points_to_quality_level_for_negative_value_is_none(#[case] skill_points: i32) {
        assert_that!(SkillPoints::new(skill_points).quality_level()).is_none();
    }

    #[rstest]
    #[case(0, QualityLevel::ONE)]
    #[case(1, QualityLevel::ONE)]
    #[case(2, QualityLevel::ONE)]
    #[case(3, QualityLevel::ONE)]
    #[case(4, QualityLevel::TWO)]
    #[case(5, QualityLevel::TWO)]
    #[case(6, QualityLevel::TWO)]
    #[case(7, QualityLevel::THREE)]
    #[case(8, QualityLevel::THREE)]
    #[case(9, QualityLevel::THREE)]
    #[case(10, QualityLevel::FOUR)]
    #[case(11, QualityLevel::FOUR)]
    #[case(12, QualityLevel::FOUR)]
    #[case(13, QualityLevel::FIVE)]
    #[case(14, QualityLevel::FIVE)]
    #[case(15, QualityLevel::FIVE)]
    #[case(16, QualityLevel::SIX)]
    #[case(17, QualityLevel::SIX)]
    #[case(18, QualityLevel::SIX)]
    #[case(19, QualityLevel::SIX)]
    #[case(100, QualityLevel::SIX)]
    fn skill_points_to_quality_level_for_non_negative_skill_points_works(
        #[case] skill_points: i32,
        #[case] expected_quality_level: QualityLevel
    ) {
        assert_that!(SkillPoints::new(skill_points).quality_level())
            .contains(expected_quality_level);
    }

    #[test]
    fn adding_skill_points_works() {
        assert_that!(SkillPoints::new(2) + SkillPoints::new(-5)).is_equal_to(SkillPoints::new(-3));
    }

    #[test]
    fn subtracting_skill_points_works() {
        assert_that!(SkillPoints::new(2) - SkillPoints::new(5)).is_equal_to(SkillPoints::new(-3));
    }

    #[rstest]
    #[case::empty([], 0)]
    #[case::singleton([5], 5)]
    #[case::multiple([3, 4, -2, 5], 10)]
    fn summing_skill_points_works(
        #[case] skill_points: impl IntoIterator<Item = i32>,
        #[case] expected_sum: i32
    ) {
        let sum = skill_points.into_iter().map(SkillPoints::new).sum();

        assert_that!(sum).is_equal_to(SkillPoints::new(expected_sum));
    }

    #[rstest]
    #[case::equal(5, 5)]
    #[case::attribute_slightly_greater(15, 14)]
    #[case::attribute_much(25, 1)]
    fn attribute_missing_skill_points_if_greater_or_equal_roll_is_zero(
        #[case] attribute: i32,
        #[case] roll: u8
    ) {
        let attribute = Attribute::new(attribute);
        let roll = Roll::new(roll).unwrap();

        assert_that!(attribute.missing_skill_points(roll)).is_equal_to(SkillPoints::new(0));
    }

    #[rstest]
    #[case::min_roll_on_attribute_zero(0, 1, 1)]
    #[case::negative_attribute(-3, 5, 8)]
    #[case::max_roll_on_positive_attribute(13, 20, 7)]
    fn attribute_missing_skill_points_if_less_than_roll_is_difference(
        #[case] attribute: i32,
        #[case] roll: u8,
        #[case] expected_missing_skill_points: i32
    ) {
        let attribute = Attribute::new(attribute);
        let roll = Roll::new(roll).unwrap();

        assert_that!(attribute.missing_skill_points(roll))
            .is_equal_to(SkillPoints::new(expected_missing_skill_points));
    }

    #[rstest]
    #[case(QualityLevel::ONE)]
    #[case(QualityLevel::TWO)]
    #[case(QualityLevel::THREE)]
    #[case(QualityLevel::FOUR)]
    #[case(QualityLevel::FIVE)]
    #[case(QualityLevel::SIX)]
    fn quality_level_saturating_add_none_is_self(#[case] quality_level: QualityLevel) {
        assert_that!(quality_level.saturating_add_option(Option::None)).is_equal_to(quality_level);
    }

    #[rstest]
    #[case(QualityLevel::ONE, QualityLevel::ONE, QualityLevel::TWO)]
    #[case(QualityLevel::ONE, QualityLevel::THREE, QualityLevel::FOUR)]
    #[case(QualityLevel::FOUR, QualityLevel::TWO, QualityLevel::SIX)]
    #[case(QualityLevel::ONE, QualityLevel::SIX, QualityLevel::SIX)]
    #[case(QualityLevel::SIX, QualityLevel::ONE, QualityLevel::SIX)]
    #[case(QualityLevel::FOUR, QualityLevel::THREE, QualityLevel::SIX)]
    #[case(QualityLevel::SIX, QualityLevel::SIX, QualityLevel::SIX)]
    fn quality_level_saturating_add_some(
        #[case] lhs: QualityLevel,
        #[case] rhs: QualityLevel,
        #[case] expected: QualityLevel,
    ) {
        assert_that!(lhs.saturating_add_option(Some(rhs))).is_equal_to(expected);
    }
}
