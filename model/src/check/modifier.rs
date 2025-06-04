use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::num::NonZeroUsize;

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
            .for_each(|modifier| result.add(modifier));
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

    pub fn add(&mut self, modifier: Modifier) {
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
}
