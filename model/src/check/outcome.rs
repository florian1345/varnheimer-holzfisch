use std::collections::HashMap;
use std::ops::{Mul, MulAssign};

use crate::probability::Probability;
use crate::skill::QualityLevel;

/// The gameplay-related kind of outcome, i.e., whether it was successful and with what quality
/// level.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SkillCheckOutcomeKind {
    SpectacularFailure,
    CriticalFailure,
    Failure,
    Success(QualityLevel),
    CriticalSuccess(QualityLevel),
    SpectacularSuccess(QualityLevel),
}

/// Contains all information necessary to evaluate the outcome of a skill check. This includes the
/// [SkillCheckOutcomeKind] as well as surrounding information such as the number of remaining fate
/// points.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckOutcome {
    pub kind: SkillCheckOutcomeKind,
    pub remaining_fate_points: usize,
}

impl SkillCheckOutcome {
    pub fn quality_level(self) -> Option<QualityLevel> {
        match self.kind {
            SkillCheckOutcomeKind::Success(ql)
            | SkillCheckOutcomeKind::CriticalSuccess(ql)
            | SkillCheckOutcomeKind::SpectacularSuccess(ql) => Some(ql),
            _ => None,
        }
    }

    pub fn is_critical_failure(self) -> bool {
        matches!(
            self.kind,
            SkillCheckOutcomeKind::CriticalFailure | SkillCheckOutcomeKind::SpectacularFailure
        )
    }

    pub fn is_improvable_success(self) -> bool {
        self.quality_level()
            .map(|quality_level| quality_level != QualityLevel::SIX)
            .unwrap_or(false)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SkillCheckOutcomeProbabilities {
    map: HashMap<SkillCheckOutcome, Probability>,
}

impl SkillCheckOutcomeProbabilities {
    pub fn of_known_outcome(outcome: SkillCheckOutcome) -> SkillCheckOutcomeProbabilities {
        SkillCheckOutcomeProbabilities {
            map: [(outcome, Probability::ONE)].into(),
        }
    }

    pub fn outcomes(&self) -> impl Iterator<Item = (SkillCheckOutcome, Probability)> + use<'_> {
        self.map
            .iter()
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
    T: IntoIterator<Item = (SkillCheckOutcome, Probability)>,
{
    fn from(value: T) -> SkillCheckOutcomeProbabilities {
        SkillCheckOutcomeProbabilities {
            map: value.into_iter().collect(),
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

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;
    use crate::check::outcome::SkillCheckOutcomeKind::{
        CriticalFailure,
        CriticalSuccess,
        Failure,
        SpectacularFailure,
        SpectacularSuccess,
        Success,
    };
    use crate::probability::Probability;
    use crate::skill::QualityLevel;

    #[rstest]
    #[case(Success(QualityLevel::ONE))]
    #[case(CriticalSuccess(QualityLevel::TWO))]
    #[case(SpectacularSuccess(QualityLevel::FIVE))]
    fn skill_check_outcome_is_improvable_success_true(#[case] kind: SkillCheckOutcomeKind) {
        let outcome = SkillCheckOutcome {
            kind,
            remaining_fate_points: 0,
        };

        assert_that!(outcome.is_improvable_success()).is_true();
    }

    #[rstest]
    #[case(Failure)]
    #[case(CriticalFailure)]
    #[case(SpectacularFailure)]
    #[case(Success(QualityLevel::SIX))]
    #[case(CriticalSuccess(QualityLevel::SIX))]
    #[case(SpectacularSuccess(QualityLevel::SIX))]
    fn skill_check_outcome_is_improvable_success_false(#[case] kind: SkillCheckOutcomeKind) {
        let outcome = SkillCheckOutcome {
            kind,
            remaining_fate_points: 0,
        };

        assert_that!(outcome.is_improvable_success()).is_false();
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
        kind: Success(QualityLevel::ONE),
        remaining_fate_points: 1,
    };
    const OUTCOME_3: SkillCheckOutcome = SkillCheckOutcome {
        kind: CriticalSuccess(QualityLevel::TWO),
        remaining_fate_points: 0,
    };

    #[test]
    fn skill_check_outcome_probabilities_mul_works_for_non_empty_map() {
        let skill_check_probabilities =
            SkillCheckOutcomeProbabilities::from([(OUTCOME_1, prob(0.2)), (OUTCOME_2, prob(0.4))]);

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
    fn skill_check_outcome_probabilities_saturating_add_works(
        #[case] lhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] rhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] expected: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
    ) {
        let mut lhs = SkillCheckOutcomeProbabilities::from(lhs);
        let rhs = SkillCheckOutcomeProbabilities::from(rhs);
        let expected = SkillCheckOutcomeProbabilities::from(expected);

        lhs.saturating_add_assign(&rhs);
        let epsilon = 0.001;

        assert_that!(lhs).is_close_to(expected, epsilon);
    }
}
