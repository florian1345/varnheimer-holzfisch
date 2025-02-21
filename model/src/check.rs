use crate::probability::Probability;
use crate::roll::Roll;
use crate::skill::{Attribute, QualityLevel, QualityLevelMap, SkillPoints};

use std::ops::{Mul, MulAssign};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckOutcome {
    SpectacularFailure,
    CriticalFailure,
    Failure,
    Success(QualityLevel),
    CriticalSuccess(QualityLevel),
    SpectacularSuccess(QualityLevel)
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct SkillCheckProbabilities {
    pub spectacular_failure_probability: Probability,
    pub critical_failure_probability: Probability,
    pub failure_probability: Probability,
    pub success_probabilities_by_quality_level: QualityLevelMap<Probability>,
    pub critical_success_probabilities_by_quality_level: QualityLevelMap<Probability>,
    pub spectacular_success_probabilities_by_quality_level: QualityLevelMap<Probability>
}

fn probability_1_for_quality_level(quality_level: QualityLevel) -> QualityLevelMap<Probability> {
    let mut result = QualityLevelMap::default();
    result[quality_level] = Probability::ONE;
    result
}

fn saturating_add_quality_level_maps(
    lhs: QualityLevelMap<Probability>,
    rhs: QualityLevelMap<Probability>
) -> QualityLevelMap<Probability> {
    let mut result = QualityLevelMap::default();

    for i in QualityLevel::ALL {
        result[i] = lhs[i].saturating_add(rhs[i]);
    }

    result
}

impl SkillCheckProbabilities {

    pub fn of_known_outcome(outcome: SkillCheckOutcome) -> SkillCheckProbabilities {
        match outcome {
            SkillCheckOutcome::SpectacularFailure =>
                SkillCheckProbabilities::spectacular_failure(),
            SkillCheckOutcome::CriticalFailure => SkillCheckProbabilities::critical_failure(),
            SkillCheckOutcome::Failure => SkillCheckProbabilities::failure(),
            SkillCheckOutcome::Success(quality_level) =>
                SkillCheckProbabilities::success(quality_level),
            SkillCheckOutcome::CriticalSuccess(quality_level) =>
                SkillCheckProbabilities::critical_success(quality_level),
            SkillCheckOutcome::SpectacularSuccess(quality_level) =>
                SkillCheckProbabilities::spectacular_success(quality_level)
        }
    }

    pub fn spectacular_failure() -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            spectacular_failure_probability: Probability::ONE,
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn critical_failure() -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            critical_failure_probability: Probability::ONE,
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn failure() -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            failure_probability: Probability::ONE,
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn spectacular_success(quality_level: QualityLevel) -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            spectacular_success_probabilities_by_quality_level:
                probability_1_for_quality_level(quality_level),
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn critical_success(quality_level: QualityLevel) -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            critical_success_probabilities_by_quality_level:
                probability_1_for_quality_level(quality_level),
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn success(quality_level: QualityLevel) -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            success_probabilities_by_quality_level: probability_1_for_quality_level(quality_level),
            ..SkillCheckProbabilities::default()
        }
    }

    pub fn saturating_add(self, other: SkillCheckProbabilities) -> SkillCheckProbabilities {
        SkillCheckProbabilities {
            spectacular_failure_probability:
                self.spectacular_failure_probability
                    .saturating_add(other.spectacular_failure_probability),
            critical_failure_probability:
                self.critical_failure_probability
                    .saturating_add(other.critical_failure_probability),
            failure_probability: self.failure_probability.saturating_add(other.failure_probability),
            success_probabilities_by_quality_level:
                saturating_add_quality_level_maps(
                    self.success_probabilities_by_quality_level,
                    other.success_probabilities_by_quality_level
                ),
            critical_success_probabilities_by_quality_level:
                saturating_add_quality_level_maps(
                    self.critical_success_probabilities_by_quality_level,
                    other.critical_success_probabilities_by_quality_level
                ),
            spectacular_success_probabilities_by_quality_level:
                saturating_add_quality_level_maps(
                    self.spectacular_success_probabilities_by_quality_level,
                    other.spectacular_success_probabilities_by_quality_level
                )
        }
    }
}

impl MulAssign<Probability> for SkillCheckProbabilities {
    fn mul_assign(&mut self, rhs: Probability) {
        self.spectacular_failure_probability *= rhs;
        self.critical_failure_probability *= rhs;
        self.failure_probability *= rhs;

        let times_rhs = |probability: &mut Probability| *probability *= rhs;

        self.success_probabilities_by_quality_level.iter_mut().for_each(times_rhs);
        self.critical_success_probabilities_by_quality_level.iter_mut().for_each(times_rhs);
        self.spectacular_success_probabilities_by_quality_level.iter_mut()
            .for_each(times_rhs);
    }
}

impl Mul<Probability> for SkillCheckProbabilities {
    type Output = SkillCheckProbabilities;

    fn mul(mut self, rhs: Probability) -> SkillCheckProbabilities {
        self *= rhs;
        self
    }
}

pub const DICE_PER_SKILL_CHECK: usize = 3;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub rolls: [Roll; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints
}

impl SkillCheckState {

    pub fn legal_actions(self) -> Vec<SkillCheckAction> {
        vec![SkillCheckAction::Accept]
    }

    pub fn current_outcome(self) -> SkillCheckOutcome {
        let max_rolls = self.rolls.iter()
            .filter(|&&roll| roll == Roll::MAX)
            .count();

        if max_rolls == DICE_PER_SKILL_CHECK {
            SkillCheckOutcome::SpectacularFailure
        }
        else if max_rolls == DICE_PER_SKILL_CHECK - 1 {
            SkillCheckOutcome::CriticalFailure
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

            if min_rolls == DICE_PER_SKILL_CHECK {
                SkillCheckOutcome::SpectacularSuccess(quality_level.unwrap_or(QualityLevel::ONE))
            }
            else if min_rolls == DICE_PER_SKILL_CHECK - 1 {
                SkillCheckOutcome::CriticalSuccess(quality_level.unwrap_or(QualityLevel::ONE))
            }
            else if let Some(quality_level) = quality_level {
                SkillCheckOutcome::Success(quality_level)
            }
            else {
                SkillCheckOutcome::Failure
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckAction {
    Accept
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct PartialSkillCheckState {
    pub attributes: [Attribute; DICE_PER_SKILL_CHECK],
    pub fixed_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    pub skill_value: SkillPoints
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
            skill_value: self.skill_value
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use super::SkillCheckOutcome::*;

    use kernal::prelude::*;

    use rstest::rstest;
    use crate::test_util::create_quality_level_map;

    fn roll(roll: u8) -> Roll {
        Roll::new(roll).unwrap()
    }

    fn skill(
        attributes: [i32; DICE_PER_SKILL_CHECK],
        skill_value: i32
    ) -> SkillCheckOutcomeScenarioFluentSkill {
        SkillCheckOutcomeScenarioFluentSkill {
            attributes,
            skill_value
        }
    }

    struct SkillCheckOutcomeScenarioFluentSkill {
        attributes: [i32; DICE_PER_SKILL_CHECK],
        skill_value: i32
    }

    impl SkillCheckOutcomeScenarioFluentSkill {
        fn roll(self, rolls: [u8; DICE_PER_SKILL_CHECK]) -> SkillCheckOutcomeScenarioFluentRoll {
            SkillCheckOutcomeScenarioFluentRoll {
                attributes: self.attributes,
                skill_value: self.skill_value,
                rolls
            }
        }
    }

    struct SkillCheckOutcomeScenarioFluentRoll {
        attributes: [i32; DICE_PER_SKILL_CHECK],
        rolls: [u8; DICE_PER_SKILL_CHECK],
        skill_value: i32
    }

    impl SkillCheckOutcomeScenarioFluentRoll {
        fn has_outcome(self, expected_outcome: SkillCheckOutcome) -> SkillCheckOutcomeScenario {
            SkillCheckOutcomeScenario {
                attributes: self.attributes,
                rolls: self.rolls,
                skill_value: self.skill_value,
                expected_outcome
            }
        }
    }

    struct SkillCheckOutcomeScenario {
        attributes: [i32; DICE_PER_SKILL_CHECK],
        rolls: [u8; DICE_PER_SKILL_CHECK],
        skill_value: i32,
        expected_outcome: SkillCheckOutcome
    }

    impl SkillCheckOutcomeScenario {
        fn skill_check_state(&self) -> SkillCheckState {
            SkillCheckState {
                attributes: [
                    Attribute::new(self.attributes[0]),
                    Attribute::new(self.attributes[1]),
                    Attribute::new(self.attributes[2])
                ],
                rolls: [roll(self.rolls[0]), roll(self.rolls[1]), roll(self.rolls[2])],
                skill_value: SkillPoints::new(self.skill_value)
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
    #[case(skill([15, 20, 25], 10).roll([20, 20, 20]).has_outcome(SpectacularFailure))]
    #[case(skill([12, 15, 18], 5).roll([1, 20, 20]).has_outcome(CriticalFailure))]
    #[case(skill([15, 15, 14], 15).roll([20, 7, 20]).has_outcome(CriticalFailure))]
    #[case(skill([8, 8, 8], 0).roll([20, 20, 19]).has_outcome(CriticalFailure))]
    #[case(skill([12, 13, 14], 4).roll([12, 3, 19]).has_outcome(Failure))]
    #[case(skill([15, 13, 15], 0).roll([12, 16, 14]).has_outcome(Failure))]
    #[case(skill([12, 14, 15], 5).roll([20, 7, 1]).has_outcome(Failure))]
    #[case(skill([13, 11, 11], 1).roll([15, 16, 13]).has_outcome(Failure))]
    #[case(skill([13, 11, 11], 8).roll([15, 16, 13]).has_outcome(Failure))]
    #[case(skill([10, 12, 14], 5).roll([11, 14, 16]).has_outcome(Success(QL_ONE)))]
    #[case(skill([11, 12, 13], 3).roll([2, 2, 2]).has_outcome(Success(QL_ONE)))]
    #[case(skill([13, 13, 15], 5).roll([14, 5, 15]).has_outcome(Success(QL_TWO)))]
    #[case(skill([8, 10, 12], 9).roll([8, 10, 12]).has_outcome(Success(QL_THREE)))]
    #[case(skill([0, 0, -1], 16).roll([2, 2, 1]).has_outcome(Success(QL_FOUR)))]
    #[case(skill([14, 14, 15], 13).roll([1, 10, 10]).has_outcome(Success(QL_FIVE)))]
    #[case(skill([10, 10, 10], 16).roll([10, 1, 10]).has_outcome(Success(QL_SIX)))]
    #[case(skill([8, 9, 10], 2).roll([11, 1, 1]).has_outcome(CriticalSuccess(QL_ONE)))]
    #[case(skill([11, 12, 13], 7).roll([1, 13, 1]).has_outcome(CriticalSuccess(QL_TWO)))]
    #[case(skill([14, 14, 15], 13).roll([1, 1, 12]).has_outcome(CriticalSuccess(QL_FIVE)))]
    #[case(skill([10, 10, 0], 0).roll([1, 1, 1]).has_outcome(SpectacularSuccess(QL_ONE)))]
    #[case(skill([1, 1, 1], 0).roll([1, 1, 1]).has_outcome(SpectacularSuccess(QL_ONE)))]
    #[case(skill([11, 12, 13], 10).roll([1, 1, 1]).has_outcome(SpectacularSuccess(QL_FOUR)))]
    fn skill_check_state_outcome(#[case] scenario: SkillCheckOutcomeScenario) {
        let skill_check_state = scenario.skill_check_state();

        assert_that!(skill_check_state.current_outcome()).is_equal_to(scenario.expected_outcome);
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
            skill_value
        };

        assert_that!(partial_skill_check_state.as_skill_check_state()).contains(SkillCheckState {
            attributes,
            rolls: [roll(7), roll(8), roll(9)],
            skill_value
        });
    }

    fn prob(prob: f64) -> Probability {
        Probability::new(prob).unwrap()
    }

    #[test]
    fn skill_check_probabilities_mul_works() {
        let skill_check_probabilities = SkillCheckProbabilities {
            spectacular_failure_probability: prob(0.02),
            critical_failure_probability: prob(0.04),
            failure_probability: prob(0.06),
            success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.08), prob(0.1), prob(0.12), prob(0.14), prob(0.16), prob(0.18)
            ]),
            critical_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.20), prob(0.22), prob(0.24), prob(0.26), prob(0.28), prob(0.30)
            ]),
            spectacular_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.32), prob(0.34), prob(0.36), prob(0.38), prob(0.40), prob(0.42)
            ])
        };

        let actual = skill_check_probabilities * prob(0.5);
        let epsilon = 0.001;

        assert_that!(actual.spectacular_failure_probability).is_close_to(prob(0.01), epsilon);
        assert_that!(actual.critical_failure_probability).is_close_to(prob(0.02), epsilon);
        assert_that!(actual.failure_probability).is_close_to(prob(0.03), epsilon);
        assert_that!(actual.success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(0.04), prob(0.05), prob(0.06), prob(0.07), prob(0.08), prob(0.09)], epsilon);
        assert_that!(actual.critical_success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(0.10), prob(0.11), prob(0.12), prob(0.13), prob(0.14), prob(0.15)], epsilon);
        assert_that!(actual.spectacular_success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(0.16), prob(0.17), prob(0.18), prob(0.19), prob(0.20), prob(0.21)], epsilon);
    }

    #[test]
    fn skill_check_saturating_add_works() {
        let lhs = SkillCheckProbabilities {
            spectacular_failure_probability: prob(0.0),
            critical_failure_probability: prob(0.5),
            failure_probability: prob(0.3),
            success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.0), prob(0.5), prob(0.9), prob(0.4), prob(0.2), prob(0.7)
            ]),
            critical_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.1), prob(0.2), prob(0.3), prob(0.4), prob(0.5), prob(0.6)
            ]),
            spectacular_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.0), prob(0.1), prob(0.2), prob(0.3), prob(0.4), prob(0.5)
            ])
        };
        let rhs = SkillCheckProbabilities {
            spectacular_failure_probability: prob(0.0),
            critical_failure_probability: prob(0.4),
            failure_probability: prob(0.8),
            success_probabilities_by_quality_level: create_quality_level_map([
                prob(1.0), prob(0.0), prob(0.1), prob(0.4), prob(0.9), prob(0.4)
            ]),
            critical_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.1), prob(0.2), prob(0.3), prob(0.4), prob(0.5), prob(0.6)
            ]),
            spectacular_success_probabilities_by_quality_level: create_quality_level_map([
                prob(0.0), prob(0.1), prob(0.2), prob(0.3), prob(0.4), prob(0.5)
            ])
        };

        let actual = lhs.saturating_add(rhs);
        let epsilon = 0.001;

        assert_that!(actual.spectacular_failure_probability).is_close_to(prob(0.0), epsilon);
        assert_that!(actual.critical_failure_probability).is_close_to(prob(0.9), epsilon);
        assert_that!(actual.failure_probability).is_close_to(prob(1.0), epsilon);
        assert_that!(actual.success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(1.0), prob(0.5), prob(1.0), prob(0.8), prob(1.0), prob(1.0)], epsilon);
        assert_that!(actual.critical_success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(0.2), prob(0.4), prob(0.6), prob(0.8), prob(1.0), prob(1.0)], epsilon);
        assert_that!(actual.spectacular_success_probabilities_by_quality_level)
            .contains_exactly_in_given_order_close_to(
                [prob(0.0), prob(0.2), prob(0.4), prob(0.6), prob(0.8), prob(1.0)], epsilon);
    }
}
