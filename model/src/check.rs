use crate::probability::Probability;
use crate::roll::Roll;
use crate::skill::{Attribute, QualityLevel, QualityLevelMap, SkillPoints};

use std::ops::{Add, AddAssign, Mul, MulAssign};

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

fn checked_add_quality_level_maps(
    lhs: QualityLevelMap<Probability>,
    rhs: QualityLevelMap<Probability>
) -> Option<QualityLevelMap<Probability>> {
    let mut result = QualityLevelMap::default();

    for i in QualityLevel::ALL {
        result[i] = lhs[i].checked_add(rhs[i])?;
    }

    Some(result)
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

    pub fn checked_add(self, other: SkillCheckProbabilities) -> Option<SkillCheckProbabilities> {
        Some(SkillCheckProbabilities {
            spectacular_failure_probability:
                self.spectacular_failure_probability
                    .checked_add(other.spectacular_failure_probability)?,
            critical_failure_probability:
                self.critical_failure_probability.checked_add(other.critical_failure_probability)?,
            failure_probability: self.failure_probability.checked_add(other.failure_probability)?,
            success_probabilities_by_quality_level:
                checked_add_quality_level_maps(
                    self.success_probabilities_by_quality_level,
                    other.success_probabilities_by_quality_level
                )?,
            critical_success_probabilities_by_quality_level:
                checked_add_quality_level_maps(
                    self.critical_success_probabilities_by_quality_level,
                    other.critical_success_probabilities_by_quality_level
                )?,
            spectacular_success_probabilities_by_quality_level:
                checked_add_quality_level_maps(
                    self.spectacular_success_probabilities_by_quality_level,
                    other.spectacular_success_probabilities_by_quality_level
                )?
        })
    }
}

impl AddAssign for SkillCheckProbabilities {
    fn add_assign(&mut self, rhs: SkillCheckProbabilities) {
        *self = *self + rhs;
    }
}

impl Add for SkillCheckProbabilities {
    type Output = SkillCheckProbabilities;

    fn add(self, rhs: SkillCheckProbabilities) -> SkillCheckProbabilities {
        self.checked_add(rhs).unwrap()
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
    pub skill_points: SkillPoints
}

impl PartialSkillCheckState {

    pub fn as_skill_check_state(self) -> Option<SkillCheckState> {
        let mut rolls = [Roll::MIN; DICE_PER_SKILL_CHECK];

        for i in 0..DICE_PER_SKILL_CHECK {
            rolls[i] = self.fixed_rolls[i]?;
        }

        Some(SkillCheckState {
            attributes: self.attributes,
            rolls,
            skill_value: self.skill_points
        })
    }
}
