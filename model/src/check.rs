use std::collections::HashMap;
use crate::probability::Probability;
use crate::roll::Roll;
use crate::skill::{Attribute, QualityLevel, SkillPoints};

use std::ops::{Mul, MulAssign};

/// The gameplay-related kind of outcome, i.e., whether it was successful and with what quality
/// level.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckOutcomeKind {
    SpectacularFailure,
    CriticalFailure,
    Failure,
    Success(QualityLevel),
    CriticalSuccess(QualityLevel),
    SpectacularSuccess(QualityLevel)
}

/// Contains all information necessary to evaluate the outcome of a skill check. This includes the
/// [SkillCheckOutcomeKind] as well as surrounding information such as the number of remaining fate
/// points.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckOutcome {
    pub kind: SkillCheckOutcomeKind,
    pub remaining_fate_points: usize
}

impl SkillCheckOutcome {

    pub fn quality_level(self) -> Option<QualityLevel> {
        match self.kind {
            SkillCheckOutcomeKind::Success(ql)
            | SkillCheckOutcomeKind::CriticalSuccess(ql)
            | SkillCheckOutcomeKind::SpectacularSuccess(ql) => Some(ql),
            _ => None
        }
    }

    pub fn is_critical_failure(self) -> bool {
        matches!(self.kind,
            SkillCheckOutcomeKind::CriticalFailure | SkillCheckOutcomeKind::SpectacularFailure)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SkillCheckOutcomeProbabilities {
    map: HashMap<SkillCheckOutcome, Probability>
}

impl SkillCheckOutcomeProbabilities {

    pub fn of_known_outcome(outcome: SkillCheckOutcome) -> SkillCheckOutcomeProbabilities {
        SkillCheckOutcomeProbabilities {
            map: [(outcome, Probability::ONE)].into()
        }
    }
    
    pub fn outcomes(&self) -> impl Iterator<Item = (SkillCheckOutcome, Probability)> + use<'_> {
        self.map.iter()
            .map(|(&outcome, &probability)| (outcome, probability))
    }

    pub fn probability_of_outcome(&self, outcome: SkillCheckOutcome) -> Probability {
        self.map.get(&outcome).cloned().unwrap_or(Probability::ZERO)
    }

    pub fn saturating_add_assign(&mut self, other: &SkillCheckOutcomeProbabilities) {
        for (&outcome, &probability) in &other.map {
            if let Some(value) = self.map.get_mut(&outcome) {
                *value = (*value).saturating_add(probability);
            }
            else {
                self.map.insert(outcome, probability);
            }
        }
    }
}

impl<T> From<T> for SkillCheckOutcomeProbabilities
where
    T: IntoIterator<Item = (SkillCheckOutcome, Probability)>
{
    fn from(value: T) -> SkillCheckOutcomeProbabilities {
        SkillCheckOutcomeProbabilities {
            map: value.into_iter().collect()
        }
    }
}

impl MulAssign<Probability> for SkillCheckOutcomeProbabilities {
    fn mul_assign(&mut self, rhs: Probability) {
        self.map.values_mut().for_each(|v| *v *= rhs);
    }
}

impl Mul<Probability> for SkillCheckOutcomeProbabilities {
    type Output = SkillCheckOutcomeProbabilities;

    fn mul(mut self, rhs: Probability) -> SkillCheckOutcomeProbabilities {
        self *= rhs;
        self
    }
}

pub const DICE_PER_SKILL_CHECK: usize = 3;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub rolls: [Roll; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints,
    pub fate_points: usize,
    pub quality_level_increase: Option<QualityLevel>,
}

impl SkillCheckState {

    pub fn legal_actions(self) -> Vec<SkillCheckAction> {
        let mut legal_actions = vec![SkillCheckAction::Accept];

        if self.fate_points == 0 {
            return legal_actions
        }

        let current_outcome = self.current_outcome();

        if current_outcome.is_critical_failure() {
            return legal_actions
        }

        legal_actions.extend(Reroll::ALL_OPTIONS.iter().cloned().map(SkillCheckAction::Reroll));

        if let Some(quality_level) = current_outcome.quality_level() {
            if quality_level != QualityLevel::SIX {
                legal_actions.push(SkillCheckAction::IncreaseQualityLevel);
            }
        }

        legal_actions
    }
    
    fn current_outcome_kind(self) -> SkillCheckOutcomeKind {
        let max_rolls = self.rolls.iter()
            .filter(|&&roll| roll == Roll::MAX)
            .count();

        if max_rolls == DICE_PER_SKILL_CHECK {
            SkillCheckOutcomeKind::SpectacularFailure
        }
        else if max_rolls == DICE_PER_SKILL_CHECK - 1 {
            SkillCheckOutcomeKind::CriticalFailure
        }
        else {
            let min_rolls = self.rolls.iter()
                .filter(|&&roll| roll == Roll::MIN)
                .count();
            let missing_skill_points = self.attributes.iter().cloned()
                .zip(self.rolls.iter().cloned())
                .map(|(attribute, roll)| attribute.missing_skill_points(roll))
                .sum();
            let quality_level = (self.skill_value - missing_skill_points).quality_level();

            if min_rolls >= DICE_PER_SKILL_CHECK - 1 {
                let quality_level = quality_level
                    .unwrap_or(QualityLevel::ONE)
                    .saturating_add_option(self.quality_level_increase);

                if min_rolls == DICE_PER_SKILL_CHECK {
                    SkillCheckOutcomeKind::SpectacularSuccess(quality_level)
                }
                else {
                    SkillCheckOutcomeKind::CriticalSuccess(quality_level)
                }
            }
            else if let Some(quality_level) = quality_level {
                SkillCheckOutcomeKind::Success(
                    quality_level.saturating_add_option(self.quality_level_increase)
                )
            }
            else {
                SkillCheckOutcomeKind::Failure
            }
        }
    }

    pub fn current_outcome(self) -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: self.current_outcome_kind(),
            remaining_fate_points: self.fate_points,
        }
    }
}

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
        Reroll([true, true, true])
    ];

    pub fn new(reroll_pattern: [bool; DICE_PER_SKILL_CHECK]) -> Option<Reroll> {
        if reroll_pattern.iter().all(|&b| !b) {
            None
        }
        else {
            Some(Reroll(reroll_pattern))
        }
    }

    pub fn apply(self, state: SkillCheckState) -> PartialSkillCheckState {
        let mut fixed_rolls = [None; DICE_PER_SKILL_CHECK];

        self.0.iter().cloned().enumerate()
            .zip(state.rolls.iter().cloned())
            .for_each(|((index, reroll), roll)| {
                if !reroll {
                    fixed_rolls[index] = Some(roll);
                }
            });

        PartialSkillCheckState {
            attributes: state.attributes,
            fixed_rolls,
            skill_value: state.skill_value,
            fate_points: state.fate_points - 1,
            quality_level_increase: state.quality_level_increase,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckActionResult {
    Done(SkillCheckOutcome),
    State(SkillCheckState),
    PartialState(PartialSkillCheckState),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckAction {
    Accept,
    Reroll(Reroll),
    IncreaseQualityLevel
}

impl SkillCheckAction {
    pub fn apply(self, state: SkillCheckState) -> SkillCheckActionResult {
        match self {
            SkillCheckAction::Accept => SkillCheckActionResult::Done(state.current_outcome()),
            SkillCheckAction::Reroll(reroll) =>
                SkillCheckActionResult::PartialState(reroll.apply(state)),
            SkillCheckAction::IncreaseQualityLevel =>
                SkillCheckActionResult::State(SkillCheckState {
                    quality_level_increase: state.quality_level_increase
                        .map(|old_ql_increase| old_ql_increase.saturating_add(QualityLevel::ONE))
                        .or(Some(QualityLevel::ONE)),
                    fate_points: state.fate_points - 1,
                    ..state
                }),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct PartialSkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub fixed_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints,
    pub fate_points: usize,
    pub quality_level_increase: Option<QualityLevel>,
}

impl PartialSkillCheckState {

    pub fn as_skill_check_state(self) -> Option<SkillCheckState> {
        let mut rolls = [Roll::MIN; DICE_PER_SKILL_CHECK];

        for (roll, &optional_fixed_roll) in rolls.iter_mut().zip(self.fixed_rolls.iter()) {
            *roll = optional_fixed_roll?;
        }

        Some(SkillCheckState {
            attributes: self.attributes,
            rolls,
            skill_value: self.skill_value,
            fate_points: self.fate_points,
            quality_level_increase: self.quality_level_increase,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use super::SkillCheckOutcomeKind::*;

    use kernal::prelude::*;

    use rstest::rstest;

    fn roll(roll: u8) -> Roll {
        Roll::new(roll).unwrap()
    }

    struct SkillCheckStateBuilder {
        attributes: [i32; DICE_PER_SKILL_CHECK],
        rolls: [u8; DICE_PER_SKILL_CHECK],
        skill_value: i32,
        fate_points: usize,
        quality_level_increase: Option<QualityLevel>,
    }

    fn skill(
        attributes: [i32; DICE_PER_SKILL_CHECK],
        skill_value: i32
    ) -> SkillCheckStateBuilder {
        SkillCheckStateBuilder {
            attributes,
            rolls: [1, 1, 1],
            skill_value,
            fate_points: 0,
            quality_level_increase: None,
        }
    }

    impl SkillCheckStateBuilder {
        fn roll(mut self, rolls: [u8; DICE_PER_SKILL_CHECK]) -> Self {
            self.rolls = rolls;
            self
        }

        fn quality_level_increase(mut self, quality_level_increase: QualityLevel) -> Self {
            self.quality_level_increase = Some(quality_level_increase);
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
                    Attribute::new(self.attributes[2])
                ],
                rolls: [roll(self.rolls[0]), roll(self.rolls[1]), roll(self.rolls[2])],
                skill_value: SkillPoints::new(self.skill_value),
                fate_points: self.fate_points,
                quality_level_increase: self.quality_level_increase,
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
            remaining_fate_points: usize
        ) -> SkillCheckOutcomeBuilder {
            self.remaining_fate_points = remaining_fate_points;
            self
        }

        fn build(self) -> SkillCheckOutcome {
            SkillCheckOutcome {
                kind: self.kind,
                remaining_fate_points: self.remaining_fate_points,
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
        skill([10, 10, 10], 5).roll([20, 20, 20]).quality_level_increase(QL_ONE),
        has_outcome(SpectacularFailure)
    )]
    #[case::ql_increase_critical_failure(
        skill([10, 10, 10], 5).roll([20, 10, 20]).quality_level_increase(QL_TWO),
        has_outcome(CriticalFailure)
    )]
    #[case::ql_increase_failure(
        skill([10, 10, 10], 5).roll([12, 12, 12]).quality_level_increase(QL_THREE),
        has_outcome(Failure)
    )]
    #[case::ql_increase_success(
        skill([10, 11, 12], 8).roll([14, 5, 12]).quality_level_increase(QL_ONE),
        has_outcome(Success(QL_THREE))
    )]
    #[case::ql_increase_critical_success_capped(
        skill([10, 11, 12], 11).roll([1, 1, 2]).quality_level_increase(QL_THREE),
        has_outcome(CriticalSuccess(QL_SIX))
    )]
    #[case::ql_increase_spectacular_success_capped(
        skill([10, 11, 12], 11).roll([1, 1, 1]).quality_level_increase(QL_ONE),
        has_outcome(SpectacularSuccess(QL_FIVE))
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

    #[rstest]
    #[case::first_roll_missing([None, Some(roll(1)), Some(roll(2))])]
    #[case::second_roll_missing([Some(roll(3)), None, Some(roll(4))])]
    #[case::third_roll_missing([Some(roll(3)), Some(roll(4)), None])]
    #[case::all_rolls_missing([None, None, None])]
    fn incomplete_partial_skill_check_state_as_skill_check_state_is_none(
        #[case] rolls: [Option<Roll>; DICE_PER_SKILL_CHECK]
    ) {
        let partial_skill_check_state = PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(11), Attribute::new(12)],
            fixed_rolls: rolls,
            skill_value: SkillPoints::new(12),
            fate_points: 0,
            quality_level_increase: None,
        };

        assert_that!(partial_skill_check_state.as_skill_check_state()).is_none();
    }
    
    #[test]
    fn complete_partial_skill_check_state_as_skill_check_state_works() {
        let attributes = [Attribute::new(10), Attribute::new(11), Attribute::new(12)];
        let skill_value = SkillPoints::new(12);
        let partial_skill_check_state = PartialSkillCheckState {
            attributes,
            fixed_rolls: [Some(roll(7)), Some(roll(8)), Some(roll(9))],
            skill_value,
            fate_points: 42,
            quality_level_increase: None,
        };

        assert_that!(partial_skill_check_state.as_skill_check_state()).contains(SkillCheckState {
            attributes,
            rolls: [roll(7), roll(8), roll(9)],
            skill_value,
            fate_points: 42,
            quality_level_increase: None,
        });
    }

    #[test]
    fn skill_check_legal_actions_without_fate_point() {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(8), Attribute::new(8)],
            rolls: [roll(8), roll(8), roll(8)],
            skill_value: SkillPoints::new(8),
            fate_points: 0,
            quality_level_increase: None,
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept
        ]);
    }

    #[test]
    fn skill_check_legal_actions_with_fate_point_success() {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(8), Attribute::new(8)],
            rolls: [roll(8), roll(8), roll(8)],
            skill_value: SkillPoints::new(8),
            fate_points: 1,
            quality_level_increase: None,
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            SkillCheckAction::Reroll(Reroll([false, false, true])),
            SkillCheckAction::Reroll(Reroll([false, true, false])),
            SkillCheckAction::Reroll(Reroll([false, true, true])),
            SkillCheckAction::Reroll(Reroll([true, false, false])),
            SkillCheckAction::Reroll(Reroll([true, false, true])),
            SkillCheckAction::Reroll(Reroll([true, true, false])),
            SkillCheckAction::Reroll(Reroll([true, true, true])),
            SkillCheckAction::IncreaseQualityLevel
        ]);
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
            fate_points: 1,
            quality_level_increase: None,
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept,
            SkillCheckAction::Reroll(Reroll([false, false, true])),
            SkillCheckAction::Reroll(Reroll([false, true, false])),
            SkillCheckAction::Reroll(Reroll([false, true, true])),
            SkillCheckAction::Reroll(Reroll([true, false, false])),
            SkillCheckAction::Reroll(Reroll([true, false, true])),
            SkillCheckAction::Reroll(Reroll([true, true, false])),
            SkillCheckAction::Reroll(Reroll([true, true, true]))
        ]);
    }

    #[rstest]
    #[case::critical_failure([roll(20), roll(3), roll(20)])]
    #[case::spectacular_failure([roll(20), roll(20), roll(20)])]
    fn skill_check_legal_actions_with_fate_point_critical_failure(
        #[case] rolls: [Roll; DICE_PER_SKILL_CHECK]
    ) {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(15), Attribute::new(16), Attribute::new(17)],
            rolls,
            skill_value: SkillPoints::new(18),
            fate_points: 1,
            quality_level_increase: None,
        };

        assert_that!(skill_check_state.legal_actions()).contains_exactly_in_any_order([
            SkillCheckAction::Accept
        ]);
    }

    #[rstest]
    #[case(Reroll([false, false, true]), [Some(roll(1)), Some(roll(2)), None])]
    #[case(Reroll([false, true, false]), [Some(roll(1)), None, Some(roll(3))])]
    #[case(Reroll([false, true, true]), [Some(roll(1)), None, None])]
    #[case(Reroll([true, false, false]), [None, Some(roll(2)), Some(roll(3))])]
    #[case(Reroll([true, false, true]), [None, Some(roll(2)), None])]
    #[case(Reroll([true, true, false]), [None, None, Some(roll(3))])]
    #[case(Reroll([true, true, true]), [None, None, None])]
    fn reroll_apply(
        #[case] reroll: Reroll,
        #[case] expected_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK]
    ) {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(8), Attribute::new(9), Attribute::new(10)],
            rolls: [roll(1), roll(2), roll(3)],
            skill_value: SkillPoints::new(5),
            fate_points: 2,
            quality_level_increase: None,
        };
        let action = SkillCheckAction::Reroll(reroll);

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::PartialState(applied_state) = result {
            assert_that!(applied_state.fixed_rolls).is_equal_to(expected_rolls);
            assert_that!(applied_state.fate_points).is_equal_to(1);
        }
        else {
            panic!("skill check action application has wrong kind of result");
        }
    }

    #[test]
    fn accept_apply() {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(11), Attribute::new(12)],
            rolls: [roll(13), roll(2), roll(15)],
            skill_value: SkillPoints::new(6),
            fate_points: 1,
            quality_level_increase: None,
        };

        let action = SkillCheckAction::Accept;

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::Done(outcome) = result {
            assert_that!(outcome).is_equal_to(SkillCheckOutcome {
                kind: Success(QL_ONE),
                remaining_fate_points: 1,
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
        #[case] old_quality_level_increase: Option<QualityLevel>,
        #[case] expected_quality_level_increase: QualityLevel
    ) {
        let skill_check_state = SkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(11), Attribute::new(12)],
            rolls: [roll(13), roll(2), roll(15)],
            skill_value: SkillPoints::new(6),
            fate_points: 1,
            quality_level_increase: old_quality_level_increase,
        };

        let action = SkillCheckAction::IncreaseQualityLevel;

        let result = action.apply(skill_check_state);

        if let SkillCheckActionResult::State(applied_state) = result {
            assert_that!(applied_state.quality_level_increase)
                .contains(expected_quality_level_increase);
            assert_that!(applied_state.fate_points).is_equal_to(0);
        }
    }

    fn prob(prob: f64) -> Probability {
        Probability::new(prob).unwrap()
    }

    #[test]
    fn skill_check_outcome_probabilities_mul_works_for_empty_map() {
        let skill_check_probabilities = SkillCheckOutcomeProbabilities::default();

        let actual = skill_check_probabilities * prob(0.5);

        assert_that!(&actual.map).is_empty();
    }

    const OUTCOME_1: SkillCheckOutcome = SkillCheckOutcome {
        kind: Failure,
        remaining_fate_points: 0,
    };
    const OUTCOME_2: SkillCheckOutcome = SkillCheckOutcome {
        kind: Success(QL_ONE),
        remaining_fate_points: 1,
    };
    const OUTCOME_3: SkillCheckOutcome = SkillCheckOutcome {
        kind: CriticalSuccess(QL_TWO),
        remaining_fate_points: 0,
    };

    #[test]
    fn skill_check_outcome_probabilities_mul_works_for_non_empty_map() {
        let skill_check_probabilities = SkillCheckOutcomeProbabilities::from(
            [(OUTCOME_1, prob(0.2)), (OUTCOME_2, prob(0.4))]
        );

        let actual = skill_check_probabilities * prob(0.5);
        let epsilon = 0.001;

        assert_that!(&actual.map).has_length(2);
        assert_that!(actual.probability_of_outcome(OUTCOME_1)).is_close_to(prob(0.1), epsilon);
        assert_that!(actual.probability_of_outcome(OUTCOME_2)).is_close_to(prob(0.2), epsilon);
    }

    #[rstest]
    #[case::both_empty([], [], [])]
    #[case::lhs_empty([], [(OUTCOME_1, prob(0.5))], [(OUTCOME_1, prob(0.5))])]
    #[case::rhs_empty([(OUTCOME_1, prob(0.5))], [], [(OUTCOME_1, prob(0.5))])]
    #[case::disjunctive(
        [(OUTCOME_1, prob(0.2))],
        [(OUTCOME_2, prob(0.3)), (OUTCOME_3, prob(0.5))],
        [(OUTCOME_1, prob(0.2)), (OUTCOME_2, prob(0.3)), (OUTCOME_3, prob(0.5))]
    )]
    #[case::overlap_without_saturation(
        [(OUTCOME_1, prob(0.3))],
        [(OUTCOME_1, prob(0.4)), (OUTCOME_2, prob(0.1))],
        [(OUTCOME_1, prob(0.7)), (OUTCOME_2, prob(0.1))]
    )]
    #[case::overlap_with_saturation(
        [(OUTCOME_1, prob(0.5)), (OUTCOME_2, prob(0.6))],
        [(OUTCOME_1, prob(0.5)), (OUTCOME_2, prob(0.5))],
        [(OUTCOME_1, prob(1.0)), (OUTCOME_2, prob(1.0))]
    )]
    fn skill_check_saturating_add_works(
        #[case] lhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] rhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] expected: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>
    ) {
        let mut lhs = SkillCheckOutcomeProbabilities::from(lhs);
        let rhs = SkillCheckOutcomeProbabilities::from(rhs);
        let expected = SkillCheckOutcomeProbabilities::from(expected);

        lhs.saturating_add_assign(&rhs);
        let epsilon = 0.001;

        assert_that!(lhs).is_close_to(expected, epsilon);
    }
}
