use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::num::NonZeroUsize;
use std::ops::{Add, AddAssign};

use crate::check::DICE_PER_SKILL_CHECK;
use crate::check::outcome::SkillCheckOutcome;
use crate::roll::Roll;
use crate::skill::{QualityLevel, SkillPoints};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Reroll([bool; DICE_PER_SKILL_CHECK]);

impl Reroll {
    pub const ALL_OPTIONS: [Reroll; 7] = [
        Reroll([false, false, true]),
        Reroll([false, true, false]),
        Reroll([false, true, true]),
        Reroll([true, false, false]),
        Reroll([true, false, true]),
        Reroll([true, true, false]),
        Reroll([true, true, true]),
    ];

    pub fn new(reroll_pattern: [bool; DICE_PER_SKILL_CHECK]) -> Option<Reroll> {
        if reroll_pattern.iter().all(|&b| !b) {
            None
        }
        else {
            Some(Reroll(reroll_pattern))
        }
    }

    pub fn num_dice(self) -> usize {
        self.0.into_iter().filter(|&b| b).count()
    }

    pub fn pattern(self) -> [bool; DICE_PER_SKILL_CHECK] {
        self.0
    }

    pub fn apply(
        self,
        rolls: [Roll; DICE_PER_SKILL_CHECK],
    ) -> [Option<Roll>; DICE_PER_SKILL_CHECK] {
        let mut fixed_rolls = [None; DICE_PER_SKILL_CHECK];

        self.0
            .iter()
            .cloned()
            .enumerate()
            .zip(rolls.iter().cloned())
            .for_each(|((index, reroll), roll)| {
                if !reroll {
                    fixed_rolls[index] = Some(roll);
                }
            });

        fixed_rolls
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Aptitude {
    max_dice: NonZeroUsize,
}

impl Aptitude {
    pub fn new(max_dice: usize) -> Option<Aptitude> {
        NonZeroUsize::new(max_dice)
            .filter(|max_dice| max_dice.get() <= DICE_PER_SKILL_CHECK)
            .map(|max_dice| Aptitude { max_dice })
    }

    pub fn legal_rerolls(self) -> impl Iterator<Item = Reroll> {
        Reroll::ALL_OPTIONS
            .into_iter()
            .filter(move |reroll| reroll.num_dice() <= self.max_dice.get())
    }

    pub fn max_dice(self) -> NonZeroUsize {
        self.max_dice
    }
}

/// Modifier actions are individual usages of modifiers.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ModifierAction {
    RerollByFate(Reroll),
    RerollByAptitude(Reroll),
    IncreaseSkillPoints(SkillPoints),
    IncreaseSkillPointsOnSuccess(SkillPoints),
    IncreaseQualityLevel,
}

/// Modifiers define the actions the player can take during a skill check. Each modifier can enable
/// a number of actions. For example, a fate point can be used to reroll some dice or increase the
/// quality level.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Modifier {
    FatePoint,
    Aptitude(Aptitude),
    ExtraSkillPoints(SkillPoints),
    ExtraSkillPointsOnSuccess(SkillPoints),
    ExtraQualityLevelOnSuccess,
}

impl Modifier {
    pub fn actions(self, current_outcome: &SkillCheckOutcome) -> Vec<ModifierAction> {
        if current_outcome.is_critical_failure() {
            return Vec::new();
        }

        match self {
            Modifier::FatePoint => {
                let mut legal_actions = Reroll::ALL_OPTIONS
                    .into_iter()
                    .map(ModifierAction::RerollByFate)
                    .collect::<Vec<_>>();

                if current_outcome.is_improvable_success() {
                    legal_actions.push(ModifierAction::IncreaseQualityLevel);
                }

                legal_actions
            },
            Modifier::Aptitude(aptitude) => aptitude
                .legal_rerolls()
                .map(ModifierAction::RerollByAptitude)
                .collect(),
            Modifier::ExtraSkillPoints(skill_points) => {
                if current_outcome.quality_level() != Some(QualityLevel::SIX) {
                    vec![ModifierAction::IncreaseSkillPoints(skill_points)]
                }
                else {
                    Vec::new()
                }
            },
            Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
                if current_outcome.is_improvable_success() {
                    vec![ModifierAction::IncreaseSkillPointsOnSuccess(skill_points)]
                }
                else {
                    Vec::new()
                }
            },
            Modifier::ExtraQualityLevelOnSuccess => {
                if current_outcome.is_improvable_success() {
                    vec![ModifierAction::IncreaseQualityLevel]
                }
                else {
                    Vec::new()
                }
            },
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct ModifierState {
    available_modifiers: BTreeMap<Modifier, NonZeroUsize>,
}

impl ModifierState {
    pub fn from_modifiers(modifiers: impl IntoIterator<Item = Modifier>) -> ModifierState {
        let mut result = ModifierState::default();
        modifiers
            .into_iter()
            .for_each(|modifier| result.insert(modifier));
        result
    }

    pub fn available_modifiers(&self) -> impl Iterator<Item = Modifier> + use<'_> {
        self.available_modifiers.keys().copied()
    }

    pub fn count_of(&self, modifier: Modifier) -> usize {
        self.available_modifiers
            .get(&modifier)
            .cloned()
            .map(NonZeroUsize::get)
            .unwrap_or(0)
    }

    pub fn insert(&mut self, modifier: Modifier) {
        match self.available_modifiers.entry(modifier) {
            Entry::Occupied(mut entry) => {
                let amount = *entry.get();
                let new_amount = amount.saturating_add(1);
                *entry.get_mut() = new_amount;
            },
            Entry::Vacant(entry) => {
                entry.insert(NonZeroUsize::new(1).unwrap());
            },
        }
    }

    pub fn set_count(&mut self, modifier: Modifier, count: usize) {
        if let Some(count) = NonZeroUsize::new(count) {
            self.available_modifiers.insert(modifier, count);
        }
        else {
            self.available_modifiers.remove(&modifier);
        }
    }

    pub fn consume(&mut self, modifier: Modifier) {
        match self.available_modifiers.entry(modifier) {
            Entry::Occupied(mut entry) => {
                let amount = *entry.get();

                if let Some(new_amount) = NonZeroUsize::new(amount.get() - 1) {
                    *entry.get_mut() = new_amount;
                }
                else {
                    entry.remove();
                }
            },
            Entry::Vacant(_) => panic!("consumed modifier not available"),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.available_modifiers.is_empty()
    }

    pub fn retain(&mut self, predicate: impl Fn(&Modifier) -> bool) {
        self.available_modifiers
            .retain(|modifier, _| predicate(modifier));
    }

    fn decrease_counts_with(
        &mut self,
        other: &ModifierState,
        decrease: impl Fn(usize, usize) -> usize,
    ) {
        self.available_modifiers.retain(|&modifier, count| {
            match NonZeroUsize::new(decrease(count.get(), other.count_of(modifier))) {
                Some(new_count) => {
                    *count = new_count;
                    true
                },
                None => false,
            }
        })
    }

    /// Reduces the counts of all modifiers within this state by their respective count in the given
    /// `other` state. Consequently, modifiers with a count less than that in `other` are
    /// removed from this state.
    pub fn saturating_sub_assign(&mut self, other: &ModifierState) {
        self.decrease_counts_with(other, usize::saturating_sub);
    }

    /// Owned variant of [ModifierState::saturating_sub_assign].
    pub fn saturating_sub(mut self, other: &ModifierState) -> ModifierState {
        self.saturating_sub_assign(other);
        self
    }

    /// Reduces the counts of all modifiers within this state that have a greater count than in the
    /// given `other` state to the count in `other`. Modifiers that are not present in `other` at
    /// all are removed from this state.
    pub fn min_assign(&mut self, other: &ModifierState) {
        self.decrease_counts_with(other, usize::min);
    }

    /// Owned variant of [ModifierState::min_assign].
    pub fn min(mut self, other: &ModifierState) -> ModifierState {
        self.min_assign(other);
        self
    }
}

impl<'rhs> AddAssign<&'rhs ModifierState> for ModifierState {
    fn add_assign(&mut self, rhs: &'rhs ModifierState) {
        for (modifier, &added_count) in rhs.available_modifiers.iter() {
            if let Some(count) = self.available_modifiers.get_mut(modifier) {
                *count = NonZeroUsize::new(count.get() + added_count.get()).unwrap();
            }
            else {
                self.available_modifiers.insert(*modifier, added_count);
            }
        }
    }
}

impl<'rhs> Add<&'rhs ModifierState> for ModifierState {
    type Output = ModifierState;

    fn add(mut self, rhs: &'rhs ModifierState) -> ModifierState {
        self += rhs;
        self
    }
}

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case([false, false, true])]
    #[case([false, true, false])]
    #[case([true, true, false])]
    #[case([true, true, true])]
    fn reroll_new_ok(#[case] reroll_pattern: [bool; DICE_PER_SKILL_CHECK]) {
        assert_that!(Reroll::new(reroll_pattern)).contains(Reroll(reroll_pattern));
    }

    #[test]
    fn reroll_reject() {
        assert_that!(Reroll::new([false, false, false])).is_none();
    }

    #[test]
    fn modifier_state_available_modifiers_empty() {
        let state = ModifierState::default();

        assert_that!(state.available_modifiers().collect::<Vec<_>>()).is_empty();
    }

    #[test]
    fn modifier_state_available_modifiers_deduplicates() {
        let state = ModifierState::from_modifiers([Modifier::FatePoint, Modifier::FatePoint]);

        assert_that!(state.available_modifiers().collect::<Vec<_>>())
            .contains_exactly_in_any_order([Modifier::FatePoint]);
    }

    #[test]
    fn count_of_empty() {
        let state = ModifierState::default();

        assert_that!(state.count_of(Modifier::FatePoint)).is_equal_to(0);
    }

    #[test]
    fn count_of_with_only_other_modifiers() {
        let state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
            Modifier::FatePoint,
        ]);

        assert_that!(state.count_of(Modifier::Aptitude(Aptitude::new(2).unwrap()))).is_equal_to(0);
    }

    #[test]
    fn count_of_non_zero() {
        let state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
            Modifier::FatePoint,
        ]);

        assert_that!(state.count_of(Modifier::FatePoint)).is_equal_to(2);
    }

    #[rstest]
    #[case(0)]
    #[case(4)]
    #[case(usize::MAX)]
    fn aptitude_new_reject(#[case] max_dice: usize) {
        assert_that!(Aptitude::new(max_dice)).is_none();
    }

    #[rstest]
    #[case(1)]
    #[case(2)]
    #[case(3)]
    fn aptitude_new_ok(#[case] max_dice: usize) {
        let aptitude = Aptitude::new(max_dice);

        assert_that!(aptitude).is_some();
        assert_that!(aptitude.unwrap().max_dice.get()).is_equal_to(max_dice);
    }

    #[test]
    fn modifier_state_consume_multiple_present() {
        let mut state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ]);

        state.consume(Modifier::FatePoint);

        assert_that!(state.count_of(Modifier::FatePoint)).is_equal_to(1);
    }

    #[test]
    fn modifier_state_consume_one_present() {
        let mut state = ModifierState::from_modifiers([Modifier::FatePoint]);

        state.consume(Modifier::FatePoint);

        assert_that!(state.count_of(Modifier::FatePoint)).is_equal_to(0);
    }

    #[test]
    fn modifier_state_consume_none_present() {
        let mut state =
            ModifierState::from_modifiers([Modifier::Aptitude(Aptitude::new(1).unwrap())]);

        assert_that!(move || state.consume(Modifier::FatePoint))
            .panics_with_message("consumed modifier not available");
    }

    #[test]
    fn set_count_on_empty() {
        let mut state = ModifierState::default();

        state.set_count(Modifier::FatePoint, 3);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([Modifier::FatePoint; 3]));
    }

    #[test]
    fn set_count_on_non_empty() {
        let mut state = ModifierState::from_modifiers([Modifier::FatePoint]);

        state.set_count(Modifier::ExtraQualityLevelOnSuccess, 2);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::ExtraQualityLevelOnSuccess,
            Modifier::ExtraQualityLevelOnSuccess,
        ]));
    }

    #[test]
    fn set_count_already_existing() {
        let mut state = ModifierState::from_modifiers([Modifier::FatePoint]);

        state.set_count(Modifier::FatePoint, 2);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([Modifier::FatePoint; 2]));
    }

    #[test]
    fn set_count_to_zero() {
        let mut state = ModifierState::from_modifiers([Modifier::FatePoint]);

        state.set_count(Modifier::FatePoint, 0);

        assert_that!(state).is_equal_to(ModifierState::default());
    }

    #[test]
    fn set_count_to_zero_with_other_modifier() {
        let mut state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::ExtraQualityLevelOnSuccess,
        ]);

        state.set_count(Modifier::ExtraQualityLevelOnSuccess, 0);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([Modifier::FatePoint]));
    }

    #[test]
    fn retain_emtpy() {
        let mut state = ModifierState::default();

        state.retain(|_| true);

        assert_that!(state.available_modifiers().collect::<Vec<_>>()).is_empty();
    }

    #[test]
    fn retain_all_match() {
        let modifiers = [
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ];
        let mut state = ModifierState::from_modifiers(modifiers);

        state.retain(|modifier| modifier != &Modifier::ExtraQualityLevelOnSuccess);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers(modifiers));
    }

    #[test]
    fn retain_none_match() {
        let mut state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ]);

        state.retain(|modifier| modifier == &Modifier::ExtraQualityLevelOnSuccess);

        assert_that!(state.available_modifiers().collect::<Vec<_>>()).is_empty();
    }

    #[test]
    fn retain_some_match() {
        let mut state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ]);

        state.retain(|modifier| modifier == &Modifier::FatePoint);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([Modifier::FatePoint]));
    }

    #[test]
    fn retain_with_higher_multiplicity() {
        let mut state = ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ]);

        state.retain(|modifier| modifier == &Modifier::FatePoint);

        assert_that!(state).is_equal_to(ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::FatePoint,
        ]));
    }

    #[rstest]
    #[case::both_empty([], [], [])]
    #[case::lhs_empty([], [Modifier::FatePoint], [])]
    #[case::rhs_empty([Modifier::FatePoint], [], [Modifier::FatePoint])]
    #[case::equal([Modifier::FatePoint], [Modifier::FatePoint], [])]
    #[case::rhs_is_subset(
        [Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [Modifier::FatePoint],
        [Modifier::ExtraQualityLevelOnSuccess]
    )]
    #[case::rhs_is_superset(
        [Modifier::FatePoint],
        [Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [],
    )]
    #[case::rhs_has_lower_multiplicity(
        [Modifier::FatePoint; 3],
        [Modifier::FatePoint],
        [Modifier::FatePoint; 2],
    )]
    #[case::rhs_has_higher_multiplicity([Modifier::FatePoint], [Modifier::FatePoint; 2], [])]
    #[case::mixed_multiplicities(
        [Modifier::FatePoint, Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [
            Modifier::FatePoint,
            Modifier::ExtraQualityLevelOnSuccess,
            Modifier::ExtraQualityLevelOnSuccess,
        ],
        [Modifier::FatePoint],
    )]
    fn modifier_state_saturating_sub(
        #[case] lhs: impl IntoIterator<Item = Modifier>,
        #[case] rhs: impl IntoIterator<Item = Modifier>,
        #[case] expected: impl IntoIterator<Item = Modifier>,
    ) {
        let lhs = ModifierState::from_modifiers(lhs);
        let rhs = ModifierState::from_modifiers(rhs);
        let expected = ModifierState::from_modifiers(expected);

        let result = lhs.saturating_sub(&rhs);

        assert_that!(result).is_equal_to(expected);
    }

    #[rstest]
    #[case::both_empty([], [], [])]
    #[case::lhs_empty([], [Modifier::FatePoint], [])]
    #[case::rhs_empty([Modifier::FatePoint], [], [])]
    #[case::equal([Modifier::FatePoint; 2], [Modifier::FatePoint; 2], [Modifier::FatePoint; 2])]
    #[case::disjunctive(
        [Modifier::FatePoint, Modifier::FatePoint, Modifier::Aptitude(Aptitude::new(2).unwrap())],
        [Modifier::ExtraQualityLevelOnSuccess],
        [],
    )]
    #[case::rhs_is_subset(
        [Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [Modifier::FatePoint],
        [Modifier::FatePoint],
    )]
    #[case::rhs_is_superset(
        [Modifier::FatePoint],
        [Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [Modifier::FatePoint],
    )]
    #[case::rhs_has_lower_multiplicity(
        [Modifier::FatePoint; 3],
        [Modifier::FatePoint; 2],
        [Modifier::FatePoint; 2],
    )]
    #[case::rhs_has_higher_multiplicity(
        [Modifier::FatePoint; 1],
        [Modifier::FatePoint; 3],
        [Modifier::FatePoint; 1],
    )]
    #[case::mixed_multiplicities(
        [Modifier::FatePoint, Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
        [
            Modifier::FatePoint,
            Modifier::ExtraQualityLevelOnSuccess,
            Modifier::ExtraQualityLevelOnSuccess,
        ],
        [Modifier::FatePoint, Modifier::ExtraQualityLevelOnSuccess],
    )]
    fn modifier_state_min(
        #[case] lhs: impl IntoIterator<Item = Modifier>,
        #[case] rhs: impl IntoIterator<Item = Modifier>,
        #[case] expected: impl IntoIterator<Item = Modifier>,
    ) {
        let lhs = ModifierState::from_modifiers(lhs);
        let rhs = ModifierState::from_modifiers(rhs);
        let expected = ModifierState::from_modifiers(expected);

        let result = lhs.min(&rhs);

        assert_that!(result).is_equal_to(expected);
    }
}
