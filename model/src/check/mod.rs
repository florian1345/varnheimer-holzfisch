pub mod modifier;
pub mod outcome;

use std::iter;

use crate::check::modifier::{Modifier, ModifierAction, ModifierState};
use crate::check::outcome::{SkillCheckOutcome, SkillCheckOutcomeKind};
use crate::roll::Roll;
use crate::skill::{Attribute, QualityLevel, SkillPoints};

pub const DICE_PER_SKILL_CHECK: usize = 3;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub rolls: [Roll; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints,
    pub extra_quality_levels_on_success: Option<QualityLevel>,
    pub extra_skill_points_on_success: SkillPoints,
    pub modifiers: ModifierState,
}

impl SkillCheckState {
    pub fn legal_actions(&self) -> Vec<SkillCheckAction> {
        let current_outcome = self.current_outcome();

        self.modifiers
            .available_modifiers()
            .flat_map(|modifier| {
                modifier
                    .actions(&current_outcome)
                    .into_iter()
                    .map(move |action| SkillCheckAction::ConsumeModifier { modifier, action })
            })
            .chain(iter::once(SkillCheckAction::Accept))
            .collect()
    }

    fn current_outcome_kind(&self) -> SkillCheckOutcomeKind {
        let max_rolls = self.rolls.iter().filter(|&&roll| roll == Roll::MAX).count();

        if max_rolls == DICE_PER_SKILL_CHECK {
            SkillCheckOutcomeKind::SpectacularFailure
        }
        else if max_rolls == DICE_PER_SKILL_CHECK - 1 {
            SkillCheckOutcomeKind::CriticalFailure
        }
        else {
            let min_rolls = self.rolls.iter().filter(|&&roll| roll == Roll::MIN).count();
            let missing_skill_points = self
                .attributes
                .iter()
                .cloned()
                .zip(self.rolls.iter().cloned())
                .map(|(attribute, roll)| attribute.missing_skill_points(roll))
                .sum();
            let mut remaining_skill_points = self.skill_value - missing_skill_points;
            let critical_success = min_rolls >= DICE_PER_SKILL_CHECK - 1;

            if !remaining_skill_points.is_negative() || critical_success {
                remaining_skill_points += self.extra_skill_points_on_success;
            }

            let quality_level = remaining_skill_points.quality_level();

            if critical_success {
                let quality_level = quality_level
                    .unwrap_or(QualityLevel::ONE)
                    .saturating_add_option(self.extra_quality_levels_on_success);

                if min_rolls == DICE_PER_SKILL_CHECK {
                    SkillCheckOutcomeKind::SpectacularSuccess(quality_level)
                }
                else {
                    SkillCheckOutcomeKind::CriticalSuccess(quality_level)
                }
            }
            else if let Some(quality_level) = quality_level {
                SkillCheckOutcomeKind::Success(
                    quality_level.saturating_add_option(self.extra_quality_levels_on_success),
                )
            }
            else {
                SkillCheckOutcomeKind::Failure
            }
        }
    }

    pub fn current_outcome(&self) -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: self.current_outcome_kind(),
            remaining_modifiers: self.modifiers.clone(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SkillCheckActionResult {
    Done(SkillCheckOutcome),
    State(SkillCheckState),
    PartialState(PartialSkillCheckState),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckAction {
    Accept,
    ConsumeModifier {
        modifier: Modifier,
        action: ModifierAction,
    },
}

impl SkillCheckAction {
    pub fn apply(self, state: SkillCheckState) -> SkillCheckActionResult {
        match self {
            SkillCheckAction::Accept => SkillCheckActionResult::Done(state.current_outcome()),
            SkillCheckAction::ConsumeModifier { modifier, action } => {
                let mut modifiers = state.modifiers;
                modifiers.consume(modifier);

                match action {
                    ModifierAction::RerollByFate(reroll) => {
                        let fixed_rolls = reroll.apply(state.rolls);

                        SkillCheckActionResult::PartialState(PartialSkillCheckState {
                            attributes: state.attributes,
                            roll_caps: [None; DICE_PER_SKILL_CHECK],
                            fixed_rolls,
                            skill_value: state.skill_value,
                            extra_quality_levels_on_success: state.extra_quality_levels_on_success,
                            extra_skill_points_on_success: state.extra_skill_points_on_success,
                            modifiers,
                            inaptitude: false,
                        })
                    },
                    ModifierAction::RerollByAptitude(reroll) => {
                        let fixed_rolls = reroll.apply(state.rolls);
                        let mut roll_caps = [None; DICE_PER_SKILL_CHECK];

                        reroll
                            .pattern()
                            .into_iter()
                            .zip(state.rolls)
                            .map(|(reroll, roll)| Some(roll).filter(|_| reroll))
                            .enumerate()
                            .for_each(|(index, roll_cap)| roll_caps[index] = roll_cap);

                        SkillCheckActionResult::PartialState(PartialSkillCheckState {
                            attributes: state.attributes,
                            roll_caps,
                            fixed_rolls,
                            skill_value: state.skill_value,
                            extra_quality_levels_on_success: state.extra_quality_levels_on_success,
                            extra_skill_points_on_success: state.extra_skill_points_on_success,
                            modifiers,
                            inaptitude: false,
                        })
                    },
                    ModifierAction::IncreaseQualityLevel => {
                        SkillCheckActionResult::State(SkillCheckState {
                            extra_quality_levels_on_success: state
                                .extra_quality_levels_on_success
                                .map(|old_ql_increase| {
                                    old_ql_increase.saturating_add(QualityLevel::ONE)
                                })
                                .or(Some(QualityLevel::ONE)),
                            modifiers,
                            ..state
                        })
                    },
                    ModifierAction::IncreaseSkillPoints(skill_points) => {
                        SkillCheckActionResult::State(SkillCheckState {
                            skill_value: state.skill_value + skill_points,
                            modifiers,
                            ..state
                        })
                    },
                    ModifierAction::IncreaseSkillPointsOnSuccess(skill_points) => {
                        SkillCheckActionResult::State(SkillCheckState {
                            extra_skill_points_on_success: state.extra_skill_points_on_success
                                + skill_points,
                            modifiers,
                            ..state
                        })
                    },
                }
            },
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PartialSkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub roll_caps: [Option<Roll>; DICE_PER_SKILL_CHECK],
    pub fixed_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints,
    pub extra_quality_levels_on_success: Option<QualityLevel>,
    pub extra_skill_points_on_success: SkillPoints,
    pub modifiers: ModifierState,
    pub inaptitude: bool,
}

impl PartialSkillCheckState {
    pub fn into_skill_check_state(self) -> Result<SkillCheckState, PartialSkillCheckState> {
        if self.inaptitude {
            return Err(self);
        }

        let mut rolls = [Roll::MIN; DICE_PER_SKILL_CHECK];

        for (roll, &optional_fixed_roll) in rolls.iter_mut().zip(self.fixed_rolls.iter()) {
            *roll = match optional_fixed_roll {
                Some(roll) => roll,
                None => return Err(self),
            }
        }

        Ok(SkillCheckState {
            attributes: self.attributes,
            rolls,
            skill_value: self.skill_value,
            extra_quality_levels_on_success: self.extra_quality_levels_on_success,
            extra_skill_points_on_success: self.extra_skill_points_on_success,
            modifiers: self.modifiers,
        })
    }

    /// Marks the best die to be rerolled (removes its roll) and removes the inaptitude flag from
    /// this state.
    ///
    /// The best die is defined according to the VTT DSA plugin as the die with the lowest value
    /// relative to the corresponding attribute. If there is a tie, the first one of the tied dice
    /// is rerolled.
    pub fn apply_inaptitude(&mut self) {
        if !self.inaptitude {
            panic!("cannot apply inaptitude: no inaptitude required");
        }

        let die_idx = self
            .fixed_rolls
            .into_iter()
            .map(|roll| roll.expect("cannot apply inaptitude: incomplete rolls"))
            .zip(self.attributes)
            .enumerate()
            .min_by_key(|&(_, (roll, attribute))| attribute.missing_skill_points_unbounded(roll))
            .unwrap()
            .0;

        self.fixed_rolls[die_idx] = None;
        self.inaptitude = false;
    }
}

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;
    use rstest_reuse::{apply, template};

    use super::*;
    use crate::check::modifier::{Aptitude, Reroll};
    use crate::check::outcome::SkillCheckOutcomeKind::*;

    const fn roll(roll: u8) -> Roll {
        Roll::new(roll).unwrap()
    }

    struct SkillCheckStateBuilder {
        attributes: [i32; DICE_PER_SKILL_CHECK],
        rolls: [u8; DICE_PER_SKILL_CHECK],
        skill_value: i32,
        fate_points: usize,
        extra_quality_levels_on_success: Option<QualityLevel>,
        extra_skill_points_on_success: i32,
    }

    fn skill(attributes: [i32; DICE_PER_SKILL_CHECK], skill_value: i32) -> SkillCheckStateBuilder {
        SkillCheckStateBuilder {
            attributes,
            rolls: [1, 1, 1],
            skill_value,
            fate_points: 0,
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: 0,
        }
    }

    impl SkillCheckStateBuilder {
        fn roll(mut self, rolls: [u8; DICE_PER_SKILL_CHECK]) -> Self {
            self.rolls = rolls;
            self
        }

        fn extra_quality_levels_on_success(
            mut self,
            extra_quality_levels_on_success: QualityLevel,
        ) -> Self {
            self.extra_quality_levels_on_success = Some(extra_quality_levels_on_success);
            self
        }

        fn extra_skill_points_on_success(mut self, extra_skill_points_on_success: i32) -> Self {
            self.extra_skill_points_on_success = extra_skill_points_on_success;
            self
        }

        fn fate_points(mut self, fate_points: usize) -> Self {
            self.fate_points = fate_points;
            self
        }

        fn build(self) -> SkillCheckState {
            SkillCheckState {
                attributes: [
                    Attribute::new(self.attributes[0]),
                    Attribute::new(self.attributes[1]),
                    Attribute::new(self.attributes[2]),
                ],
                rolls: [
                    roll(self.rolls[0]),
                    roll(self.rolls[1]),
                    roll(self.rolls[2]),
                ],
                skill_value: SkillPoints::new(self.skill_value),
                extra_quality_levels_on_success: self.extra_quality_levels_on_success,
                extra_skill_points_on_success: SkillPoints::new(self.extra_skill_points_on_success),
                modifiers: ModifierState::from_modifiers(vec![
                    Modifier::FatePoint;
                    self.fate_points
                ]),
            }
        }
    }

    struct SkillCheckOutcomeBuilder {
        kind: SkillCheckOutcomeKind,
        remaining_fate_points: usize,
    }

    fn has_outcome(kind: SkillCheckOutcomeKind) -> SkillCheckOutcomeBuilder {
        SkillCheckOutcomeBuilder {
            kind,
            remaining_fate_points: 0,
        }
    }

    impl SkillCheckOutcomeBuilder {
        fn remaining_fate_points(
            mut self,
            remaining_fate_points: usize,
        ) -> SkillCheckOutcomeBuilder {
            self.remaining_fate_points = remaining_fate_points;
            self
        }

        fn build(self) -> SkillCheckOutcome {
            SkillCheckOutcome {
                kind: self.kind,
                remaining_modifiers: ModifierState::from_modifiers(vec![
                    Modifier::FatePoint;
                    self.remaining_fate_points
                ]),
            }
        }
    }

    const QL_ONE: QualityLevel = QualityLevel::ONE;
    const QL_TWO: QualityLevel = QualityLevel::TWO;
    const QL_THREE: QualityLevel = QualityLevel::THREE;
    const QL_FOUR: QualityLevel = QualityLevel::FOUR;
    const QL_FIVE: QualityLevel = QualityLevel::FIVE;
    const QL_SIX: QualityLevel = QualityLevel::SIX;

    #[rstest]
    #[case(skill([15, 20, 25], 10).roll([20, 20, 20]), has_outcome(SpectacularFailure))]
    #[case(skill([12, 15, 18], 5).roll([1, 20, 20]), has_outcome(CriticalFailure))]
    #[case(skill([15, 15, 14], 15).roll([20, 7, 20]), has_outcome(CriticalFailure))]
    #[case(skill([8, 8, 8], 0).roll([20, 20, 19]), has_outcome(CriticalFailure))]
    #[case(skill([12, 13, 14], 4).roll([12, 3, 19]), has_outcome(Failure))]
    #[case(skill([15, 13, 15], 0).roll([12, 16, 14]), has_outcome(Failure))]
    #[case(skill([12, 14, 15], 5).roll([20, 7, 1]), has_outcome(Failure))]
    #[case(skill([13, 11, 11], 1).roll([15, 16, 13]), has_outcome(Failure))]
    #[case(skill([13, 11, 11], 8).roll([15, 16, 13]), has_outcome(Failure))]
    #[case(skill([10, 12, 14], 5).roll([11, 14, 16]), has_outcome(Success(QL_ONE)))]
    #[case(skill([11, 12, 13], 3).roll([2, 2, 2]), has_outcome(Success(QL_ONE)))]
    #[case(skill([13, 13, 15], 5).roll([14, 5, 15]), has_outcome(Success(QL_TWO)))]
    #[case(skill([8, 10, 12], 9).roll([8, 10, 12]), has_outcome(Success(QL_THREE)))]
    #[case(skill([0, 0, -1], 16).roll([2, 2, 1]), has_outcome(Success(QL_FOUR)))]
    #[case(skill([14, 14, 15], 13).roll([1, 10, 10]), has_outcome(Success(QL_FIVE)))]
    #[case(skill([10, 10, 10], 16).roll([10, 1, 10]), has_outcome(Success(QL_SIX)))]
    #[case(skill([8, 9, 10], 2).roll([11, 1, 1]), has_outcome(CriticalSuccess(QL_ONE)))]
    #[case(skill([11, 12, 13], 7).roll([1, 13, 1]), has_outcome(CriticalSuccess(QL_TWO)))]
    #[case(skill([14, 14, 15], 13).roll([1, 1, 12]), has_outcome(CriticalSuccess(QL_FIVE)))]
    #[case(skill([10, 10, 0], 0).roll([1, 1, 1]), has_outcome(SpectacularSuccess(QL_ONE)))]
    #[case(skill([1, 1, 1], 0).roll([1, 1, 1]), has_outcome(SpectacularSuccess(QL_ONE)))]
    #[case(skill([11, 12, 13], 10).roll([1, 1, 1]), has_outcome(SpectacularSuccess(QL_FOUR)))]
    #[case::ql_increase_spectacular_failure(
        skill([10, 10, 10], 5).roll([20, 20, 20]).extra_quality_levels_on_success(QL_ONE),
        has_outcome(SpectacularFailure)
    )]
    #[case::ql_increase_critical_failure(
        skill([10, 10, 10], 5).roll([20, 10, 20]).extra_quality_levels_on_success(QL_TWO),
        has_outcome(CriticalFailure)
    )]
    #[case::ql_increase_failure(
        skill([10, 10, 10], 5).roll([12, 12, 12]).extra_quality_levels_on_success(QL_THREE),
        has_outcome(Failure)
    )]
    #[case::ql_increase_success(
        skill([10, 11, 12], 8).roll([14, 5, 12]).extra_quality_levels_on_success(QL_ONE),
        has_outcome(Success(QL_THREE))
    )]
    #[case::ql_increase_critical_success_capped(
        skill([10, 11, 12], 11).roll([1, 1, 2]).extra_quality_levels_on_success(QL_THREE),
        has_outcome(CriticalSuccess(QL_SIX))
    )]
    #[case::ql_increase_spectacular_success_capped(
        skill([10, 11, 12], 11).roll([1, 1, 1]).extra_quality_levels_on_success(QL_ONE),
        has_outcome(SpectacularSuccess(QL_FIVE))
    )]
    #[case::extra_skill_points_on_success_not_applied_to_spectacular_failure(
        skill([15, 15, 15], 14).roll([20, 20, 20]).extra_skill_points_on_success(1),
        has_outcome(SpectacularFailure)
    )]
    #[case::extra_skill_points_on_success_not_applied_to_critical_failure(
        skill([15, 15, 15], 9).roll([20, 20, 4]).extra_skill_points_on_success(1),
        has_outcome(CriticalFailure)
    )]
    #[case::extra_skill_points_on_success_not_applied_to_failure(
        skill([15, 15, 15], 4).roll([20, 10, 4]).extra_skill_points_on_success(1),
        has_outcome(Failure)
    )]
    #[case::extra_skill_points_on_success_without_changing_quality(
        skill([13, 14, 15], 5).roll([15, 15, 4]).extra_skill_points_on_success(1),
        has_outcome(Success(QL_ONE))
    )]
    #[case::extra_skill_points_on_success_changing_quality(
        skill([13, 14, 15], 5).roll([15, 15, 4]).extra_skill_points_on_success(2),
        has_outcome(Success(QL_TWO))
    )]
    #[case::extra_skill_points_on_success_critical_success(
        skill([13, 14, 15], 6).roll([1, 2, 1]).extra_skill_points_on_success(1),
        has_outcome(CriticalSuccess(QL_THREE))
    )]
    #[case::extra_skill_points_on_success_spectacular_success(
        skill([13, 14, 15], 6).roll([1, 1, 1]).extra_skill_points_on_success(4),
        has_outcome(SpectacularSuccess(QL_FOUR))
    )]
    #[case::extra_skill_points_on_success_critical_success_that_would_have_failed(
        skill([10, 10, 10], 9).roll([1, 1, 20]).extra_skill_points_on_success(5),
        has_outcome(CriticalSuccess(QL_TWO))
    )]
    #[case::remaining_fate_points(
        skill([10, 10, 10], 10).roll([10, 10, 10]).fate_points(3),
        has_outcome(Success(QL_FOUR)).remaining_fate_points(3)
    )]
    fn skill_check_state_outcome(
        #[case] skill_check_state_builder: SkillCheckStateBuilder,
        #[case] skill_check_outcome_builder: SkillCheckOutcomeBuilder,
    ) {
        let skill_check_state = skill_check_state_builder.build();

        assert_that!(skill_check_state.current_outcome())
            .is_equal_to(skill_check_outcome_builder.build());
    }

    fn partial_skill_check_state_empty_no_options() -> PartialSkillCheckState {
        PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
            roll_caps: [None; DICE_PER_SKILL_CHECK],
            fixed_rolls: [None; DICE_PER_SKILL_CHECK],
            skill_value: SkillPoints::new(12),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
            modifiers: ModifierState::default(),
            inaptitude: false,
        }
    }

    #[rstest]
    #[case::first_roll_missing([None, Some(roll(1)), Some(roll(2))], false)]
    #[case::second_roll_missing([Some(roll(3)), None, Some(roll(4))], false)]
    #[case::third_roll_missing([Some(roll(3)), Some(roll(4)), None], false)]
    #[case::all_rolls_missing([None, None, None], false)]
    #[case::inaptitude([Some(roll(1)), Some(roll(2)), Some(roll(3))], true)]
    #[case::inaptitude_with_missing_roll([Some(roll(1)), None, Some(roll(3))], true)]
    fn incomplete_partial_skill_check_state_into_skill_check_state_is_none(
        #[case] rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
        #[case] inaptitude: bool,
    ) {
        let partial_skill_check_state = PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(11), Attribute::new(12)],
            fixed_rolls: rolls,
            skill_value: SkillPoints::new(12),
            inaptitude,
            ..partial_skill_check_state_empty_no_options()
        };

        assert_that!(partial_skill_check_state.clone().into_skill_check_state())
            .contains_error(partial_skill_check_state);
    }

    #[test]
    fn complete_partial_skill_check_state_into_skill_check_state_works() {
        let attributes = [Attribute::new(10), Attribute::new(11), Attribute::new(12)];
        let skill_value = SkillPoints::new(12);
        let modifiers = ModifierState::from_modifiers([Modifier::FatePoint]);
        let extra_quality_levels_on_success = Some(QL_TWO);
        let extra_skill_points_on_success = SkillPoints::new(3);
        let partial_skill_check_state = PartialSkillCheckState {
            attributes,
            fixed_rolls: [Some(roll(7)), Some(roll(8)), Some(roll(9))],
            skill_value,
            extra_quality_levels_on_success,
            extra_skill_points_on_success,
            modifiers: modifiers.clone(),
            ..partial_skill_check_state_empty_no_options()
        };

        assert_that!(partial_skill_check_state.into_skill_check_state()).contains_value(
            SkillCheckState {
                attributes,
                rolls: [roll(7), roll(8), roll(9)],
                skill_value,
                extra_quality_levels_on_success,
                extra_skill_points_on_success,
                modifiers,
            },
        );
    }

    #[test]
    fn apply_inaptitude_panics_if_no_inaptitude_is_required() {
        let mut partial_skill_check_state = PartialSkillCheckState {
            inaptitude: false,
            ..partial_skill_check_state_empty_no_options()
        };

        assert_that!(move || partial_skill_check_state.apply_inaptitude())
            .panics_with_message("cannot apply inaptitude: no inaptitude required");
    }

    #[rstest]
    #[case([None, None, None])]
    #[case([None, Some(roll(1)), Some(roll(2))])]
    #[case([Some(roll(1)), None, Some(roll(2))])]
    #[case([Some(roll(1)), Some(roll(2)), None])]
    fn apply_inaptitude_panics_if_fixed_rolls_are_incomplete(
        #[case] fixed_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    ) {
        let mut partial_skill_check_state = PartialSkillCheckState {
            inaptitude: true,
            fixed_rolls,
            ..partial_skill_check_state_empty_no_options()
        };

        assert_that!(move || partial_skill_check_state.apply_inaptitude())
            .panics_with_message("cannot apply inaptitude: incomplete rolls");
    }

    #[rstest]
    #[case::first_die([5, 11, 12], [10, 13, 14], [None, Some(11), Some(12)])]
    #[case::second_die([16, 11, 17], [13, 10, 14], [Some(16), None, Some(17)])]
    #[case::third_die([12, 13, 13], [10, 13, 14], [Some(12), Some(13), None])]
    #[case::two_way_tie([11, 14, 13], [10, 14, 13], [Some(11), None, Some(13)])]
    #[case::three_way_tie_below_attributes([11, 10, 12], [12, 11, 13], [None, Some(10), Some(12)])]
    #[case::three_way_tie_above_attributes([13, 12, 14], [12, 11, 13], [None, Some(12), Some(14)])]
    fn apply_inaptitude_works(
        #[case] rolls: [u8; DICE_PER_SKILL_CHECK],
        #[case] attributes: [i32; DICE_PER_SKILL_CHECK],
        #[case] expected: [Option<u8>; DICE_PER_SKILL_CHECK],
    ) {
        let fixed_rolls = [
            Some(roll(rolls[0])),
            Some(roll(rolls[1])),
            Some(roll(rolls[2])),
        ];
        let attributes = [
            Attribute::new(attributes[0]),
            Attribute::new(attributes[1]),
            Attribute::new(attributes[2]),
        ];
        let mut state = PartialSkillCheckState {
            fixed_rolls,
            attributes,
            inaptitude: true,
            ..partial_skill_check_state_empty_no_options()
        };

        state.apply_inaptitude();

        let expected_fixed_rolls = [
            expected[0].map(roll),
            expected[1].map(roll),
            expected[2].map(roll),
        ];

        assert_that!(state.fixed_rolls).is_equal_to(expected_fixed_rolls);
        assert_that!(state.inaptitude).is_false();
    }

    fn skill_check_state_success_no_options() -> SkillCheckState {
        SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(8), Attribute::new(8)],
            rolls: [roll(8), roll(8), roll(8)],
            skill_value: SkillPoints::new(8),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
            modifiers: ModifierState::default(),
        }
    }

    #[test]
    fn skill_check_legal_actions_without_fate_point_or_aptitude() {
        assert_that!(skill_check_state_success_no_options().legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    fn use_fate_point(action: ModifierAction) -> SkillCheckAction {
        SkillCheckAction::ConsumeModifier {
            modifier: Modifier::FatePoint,
            action,
        }
    }

    fn use_aptitude(
        max_dice: usize,
        reroll_pattern: [bool; DICE_PER_SKILL_CHECK],
    ) -> SkillCheckAction {
        SkillCheckAction::ConsumeModifier {
            modifier: Modifier::Aptitude(Aptitude::new(max_dice).unwrap()),
            action: ModifierAction::RerollByAptitude(Reroll::new(reroll_pattern).unwrap()),
        }
    }

    #[test]
    fn skill_check_legal_actions_with_fate_point_success() {
        let skill_check_state = SkillCheckState {
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, false, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, true, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, true, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, false, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, false, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, true, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, true, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::IncreaseQualityLevel),
        ]);
    }

    #[test]
    fn skill_check_legal_actions_with_aptitude() {
        let skill_check_state = SkillCheckState {
            modifiers: ModifierState::from_modifiers([Modifier::Aptitude(
                Aptitude::new(1).unwrap(),
            )]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            use_aptitude(1, [false, false, true]),
            use_aptitude(1, [false, true, false]),
            use_aptitude(1, [true, false, false]),
        ]);
    }

    #[test]
    fn skill_check_legal_actions_with_double_aptitude() {
        let skill_check_state = SkillCheckState {
            modifiers: ModifierState::from_modifiers([Modifier::Aptitude(
                Aptitude::new(2).unwrap(),
            )]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            use_aptitude(2, [false, false, true]),
            use_aptitude(2, [false, true, false]),
            use_aptitude(2, [true, false, false]),
            use_aptitude(2, [true, true, false]),
            use_aptitude(2, [true, false, true]),
            use_aptitude(2, [false, true, true]),
        ]);
    }

    #[rstest]
    #[case::critical_failure([roll(20), roll(3), roll(20)])]
    #[case::spectacular_failure([roll(20), roll(20), roll(20)])]
    fn skill_check_legal_actions_with_aptitude_critical_failure(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK],
    ) {
        let skill_check_state = SkillCheckState {
            rolls,
            modifiers: ModifierState::from_modifiers([Modifier::Aptitude(
                Aptitude::new(1).unwrap(),
            )]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    #[rstest]
    #[case::failure([roll(19), roll(19), roll(19)])]
    #[case::max_quality_level_success([roll(2), roll(3), roll(4)])]
    #[case::max_quality_level_critical([roll(1), roll(1), roll(4)])]
    #[case::max_quality_level_spectacular([roll(1), roll(1), roll(1)])]
    fn skill_check_legal_actions_with_fate_point_without_increase_quality_level(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK],
    ) {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
            rolls,
            skill_value: SkillPoints::new(18),
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, false, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, true, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([false, true, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, false, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, false, true]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, true, false]).unwrap(),
            )),
            use_fate_point(ModifierAction::RerollByFate(
                Reroll::new([true, true, true]).unwrap(),
            )),
        ]);
    }

    #[rstest]
    #[case::critical_failure([roll(20), roll(3), roll(20)])]
    #[case::spectacular_failure([roll(20), roll(20), roll(20)])]
    fn skill_check_legal_actions_with_fate_point_critical_failure(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK],
    ) {
        let skill_check_state = SkillCheckState {
            rolls,
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    #[rstest]
    #[case::failure([roll(19), roll(19), roll(19)], 5)]
    #[case::success([roll(3), roll(4), roll(5)], 5)]
    #[case::critical_success([roll(1), roll(1), roll(2)], 5)]
    #[case::spectacular_success([roll(1), roll(1), roll(1)], 5)]
    fn skill_check_legal_actions_with_extra_skill_points_applicable(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK],
        #[case] skill_value: i32,
    ) {
        let skill_points = SkillPoints::new(2);
        let modifier = Modifier::ExtraSkillPoints(skill_points);
        let skill_check_state = SkillCheckState {
            rolls,
            skill_value: SkillPoints::new(skill_value),
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            SkillCheckAction::ConsumeModifier {
                modifier,
                action: ModifierAction::IncreaseSkillPoints(skill_points),
            },
        ]);
    }

    #[rstest]
    #[case::critical_failure([roll(20), roll(3), roll(20)], 5)]
    #[case::spectacular_failure([roll(20), roll(20), roll(20)], 5)]
    #[case::max_quality_level([roll(3), roll(4), roll(5)], 18)]
    fn skill_check_legal_actions_with_extra_skill_points_not_applicable(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK],
        #[case] skill_value: i32,
    ) {
        let modifier = Modifier::ExtraSkillPoints(SkillPoints::new(1));
        let skill_check_state = SkillCheckState {
            rolls,
            skill_value: SkillPoints::new(skill_value),
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    #[test]
    fn skill_check_legal_actions_with_extra_skill_points_on_success_applicable() {
        let skill_points = SkillPoints::new(2);
        let modifier = Modifier::ExtraSkillPointsOnSuccess(skill_points);
        let skill_check_state = SkillCheckState {
            rolls: [roll(5), roll(5), roll(5)],
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            SkillCheckAction::ConsumeModifier {
                modifier,
                action: ModifierAction::IncreaseSkillPointsOnSuccess(skill_points),
            },
        ]);
    }

    #[template]
    #[rstest]
    #[case::failure(SkillCheckState {
        rolls: [roll(19), roll(19), roll(19)],
        ..skill_check_state_success_no_options()
        })]
    #[case::critical_failure(SkillCheckState {
        rolls: [roll(20), roll(19), roll(20)],
        ..skill_check_state_success_no_options()
        })]
    #[case::spectacular_failure(SkillCheckState {
        rolls: [roll(20), roll(20), roll(20)],
        ..skill_check_state_success_no_options()
        })]
    #[case::max_quality_level(SkillCheckState {
        rolls: [roll(2), roll(2), roll(2)],
        skill_value: SkillPoints::new(18),
        ..skill_check_state_success_no_options()
        })]
    #[case::max_quality_level_critical_success(SkillCheckState {
        rolls: [roll(2), roll(1), roll(1)],
        skill_value: SkillPoints::new(18),
        ..skill_check_state_success_no_options()
        })]
    #[case::max_quality_level_spectacular_success(SkillCheckState {
        rolls: [roll(1), roll(1), roll(1)],
        skill_value: SkillPoints::new(18),
        ..skill_check_state_success_no_options()
        })]
    fn failure_or_max_quality_level(#[case] skill_check_state: SkillCheckState) {}

    #[apply(failure_or_max_quality_level)]
    fn skill_check_legal_actions_with_extra_skill_points_on_success_not_applicable(
        #[case] skill_check_state: SkillCheckState,
    ) {
        let modifier = Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(1));
        let skill_check_state = SkillCheckState {
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state
        };

        assert_that!(skill_check_state.legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    #[test]
    fn skill_check_legal_actions_with_extra_quality_level_on_success_applicable() {
        let skill_check_state = SkillCheckState {
            rolls: [roll(5), roll(5), roll(5)],
            modifiers: ModifierState::from_modifiers([Modifier::ExtraQualityLevelOnSuccess]),
            ..skill_check_state_success_no_options()
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            SkillCheckAction::ConsumeModifier {
                modifier: Modifier::ExtraQualityLevelOnSuccess,
                action: ModifierAction::IncreaseQualityLevel,
            },
        ]);
    }

    #[apply(failure_or_max_quality_level)]
    fn skill_check_legal_actions_with_extra_quality_level_on_success_not_applicable(
        #[case] skill_check_state: SkillCheckState,
    ) {
        let skill_check_state = SkillCheckState {
            modifiers: ModifierState::from_modifiers([Modifier::ExtraQualityLevelOnSuccess]),
            ..skill_check_state
        };

        assert_that!(skill_check_state.legal_actions())
            .contains_exactly_in_any_order([SkillCheckAction::Accept]);
    }

    #[rstest]
    #[case([false, false, true], [Some(roll(1)), Some(roll(2)), None])]
    #[case([false, true, false], [Some(roll(1)), None, Some(roll(3))])]
    #[case([false, true, true], [Some(roll(1)), None, None])]
    #[case([true, false, false], [None, Some(roll(2)), Some(roll(3))])]
    #[case([true, false, true], [None, Some(roll(2)), None])]
    #[case([true, true, false], [None, None, Some(roll(3))])]
    #[case([true, true, true], [None, None, None])]
    fn reroll_fate_apply(
        #[case] reroll_pattern: [bool; DICE_PER_SKILL_CHECK],
        #[case] expected_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    ) {
        let aptitude_1 = Modifier::Aptitude(Aptitude::new(1).unwrap());
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(9), Attribute::new(10)],
            rolls: [roll(1), roll(2), roll(3)],
            skill_value: SkillPoints::new(5),
            modifiers: ModifierState::from_modifiers([
                Modifier::FatePoint,
                Modifier::FatePoint,
                aptitude_1,
            ]),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
        };
        let action = SkillCheckAction::ConsumeModifier {
            modifier: Modifier::FatePoint,
            action: ModifierAction::RerollByFate(Reroll::new(reroll_pattern).unwrap()),
        };

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::PartialState(applied_state) = result {
            assert_that!(applied_state.fixed_rolls).is_equal_to(expected_rolls);
            assert_that!(applied_state.modifiers).is_equal_to(ModifierState::from_modifiers([
                Modifier::FatePoint,
                aptitude_1,
            ]));
        }
        else {
            panic!("skill check action application has wrong kind of result");
        }
    }

    #[rstest]
    #[case(
        [false, false, true],
        [Some(roll(1)), Some(roll(2)), None],
        [None, None, Some(roll(3))],
    )]
    #[case(
        [false, true, false],
        [Some(roll(1)), None, Some(roll(3))],
        [None, Some(roll(2)), None],
    )]
    #[case(
        [true, false, false],
        [None, Some(roll(2)), Some(roll(3))],
        [Some(roll(1)), None, None],
    )]
    #[case(
        [true, true, true],
        [None, None, None],
        [Some(roll(1)), Some(roll(2)), Some(roll(3))],
    )]
    fn reroll_aptitude_apply(
        #[case] reroll_pattern: [bool; DICE_PER_SKILL_CHECK],
        #[case] expected_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
        #[case] expected_roll_caps: [Option<Roll>; DICE_PER_SKILL_CHECK],
    ) {
        let aptitude_2 = Modifier::Aptitude(Aptitude::new(2).unwrap());
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(9), Attribute::new(10)],
            rolls: [roll(1), roll(2), roll(3)],
            skill_value: SkillPoints::new(5),
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint, aptitude_2]),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
        };
        let action = SkillCheckAction::ConsumeModifier {
            modifier: aptitude_2,
            action: ModifierAction::RerollByAptitude(Reroll::new(reroll_pattern).unwrap()),
        };

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::PartialState(applied_state) = result {
            assert_that!(applied_state.roll_caps).is_equal_to(expected_roll_caps);
            assert_that!(applied_state.fixed_rolls).is_equal_to(expected_rolls);
            assert_that!(applied_state.modifiers)
                .is_equal_to(ModifierState::from_modifiers([Modifier::FatePoint]));
        }
        else {
            panic!("skill check action application has wrong kind of result");
        }
    }

    #[test]
    fn accept_apply() {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(12), Attribute::new(13), Attribute::new(12)],
            rolls: [roll(14), roll(2), roll(15)],
            skill_value: SkillPoints::new(6),
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
        };

        let action = SkillCheckAction::Accept;

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::Done(outcome) = result {
            assert_that!(outcome).is_equal_to(SkillCheckOutcome {
                kind: Success(QL_ONE),
                remaining_modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            });
        }
        else {
            panic!("skill check action application has wrong kind of result");
        }
    }

    #[rstest]
    #[case::without_old_ql_increase(None, QL_ONE)]
    #[case::with_old_ql_increase(Some(QL_ONE), QL_TWO)]
    #[case::with_max_ql_increase(Some(QL_SIX), QL_SIX)]
    fn increase_quality_level_apply(
        #[case] old_extra_quality_levels_on_success: Option<QualityLevel>,
        #[case] expected_extra_quality_levels_on_success: QualityLevel,
    ) {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(11), Attribute::new(12)],
            rolls: [roll(13), roll(2), roll(15)],
            skill_value: SkillPoints::new(6),
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            extra_quality_levels_on_success: old_extra_quality_levels_on_success,
            extra_skill_points_on_success: SkillPoints::new(0),
        };

        let action = SkillCheckAction::ConsumeModifier {
            modifier: Modifier::FatePoint,
            action: ModifierAction::IncreaseQualityLevel,
        };

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::State(applied_state) = result {
            assert_that!(applied_state.extra_quality_levels_on_success)
                .contains(expected_extra_quality_levels_on_success);
            assert_that!(applied_state.modifiers).is_equal_to(ModifierState::default());
        }
    }

    #[test]
    fn increase_skill_points_apply() {
        let modifier = Modifier::ExtraSkillPoints(SkillPoints::new(3));
        let extra_skill_points_on_success = SkillPoints::new(1);
        let skill_check_state = SkillCheckState {
            skill_value: SkillPoints::new(6),
            extra_skill_points_on_success,
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state_success_no_options()
        };

        let action = SkillCheckAction::ConsumeModifier {
            modifier,
            action: ModifierAction::IncreaseSkillPoints(SkillPoints::new(3)),
        };

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::State(applied_state) = result {
            assert_that!(applied_state.extra_skill_points_on_success)
                .is_equal_to(extra_skill_points_on_success);
            assert_that!(applied_state.skill_value).is_equal_to(SkillPoints::new(9));
            assert_that!(applied_state.modifiers).is_equal_to(ModifierState::default());
        }
    }

    #[test]
    fn increase_skill_points_on_success_apply() {
        let modifier = Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(3));
        let skill_value = SkillPoints::new(6);
        let skill_check_state = SkillCheckState {
            skill_value,
            extra_skill_points_on_success: SkillPoints::new(1),
            modifiers: ModifierState::from_modifiers([modifier]),
            ..skill_check_state_success_no_options()
        };

        let action = SkillCheckAction::ConsumeModifier {
            modifier,
            action: ModifierAction::IncreaseSkillPointsOnSuccess(SkillPoints::new(3)),
        };

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::State(applied_state) = result {
            assert_that!(applied_state.extra_skill_points_on_success)
                .is_equal_to(SkillPoints::new(4));
            assert_that!(applied_state.skill_value).is_equal_to(skill_value);
            assert_that!(applied_state.modifiers).is_equal_to(ModifierState::default());
        }
    }
}
