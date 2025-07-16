use std::collections::HashMap;
use std::ops::{Mul, MulAssign};

use crate::check::modifier::ModifierState;
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
/// [SkillCheckOutcomeKind] as well as information about the unused modifiers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SkillCheckOutcome {
    pub kind: SkillCheckOutcomeKind,
    pub remaining_modifiers: ModifierState,
}

impl SkillCheckOutcome {
    pub fn quality_level(&self) -> Option<QualityLevel> {
        match self.kind {
            SkillCheckOutcomeKind::Success(ql)
            | SkillCheckOutcomeKind::CriticalSuccess(ql)
            | SkillCheckOutcomeKind::SpectacularSuccess(ql) => Some(ql),
            _ => None,
        }
    }

    pub fn is_critical_failure(&self) -> bool {
        matches!(
            self.kind,
            SkillCheckOutcomeKind::CriticalFailure | SkillCheckOutcomeKind::SpectacularFailure
        )
    }

    pub fn is_improvable_success(&self) -> bool {
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

    pub fn outcomes(&self) -> impl Iterator<Item = (&SkillCheckOutcome, Probability)> {
        self.map
            .iter()
            .map(|(outcome, &probability)| (outcome, probability))
    }

    pub fn probability_of_outcome(&self, outcome: &SkillCheckOutcome) -> Probability {
        self.map.get(outcome).cloned().unwrap_or(Probability::ZERO)
    }

    pub fn saturating_add_assign(&mut self, other: &SkillCheckOutcomeProbabilities) {
        for (outcome, &probability) in &other.map {
            if let Some(value) = self.map.get_mut(outcome) {
                *value = (*value).saturating_add(probability);
            }
            else {
                self.map.insert(outcome.clone(), probability);
            }
        }
    }

    pub fn saturating_fma_assign(
        &mut self,
        other: &SkillCheckOutcomeProbabilities,
        probability: Probability,
    ) {
        for (outcome, &outcome_probability) in &other.map {
            if let Some(value) = self.map.get_mut(outcome) {
                *value = (*value).saturating_add(outcome_probability * probability);
            }
            else {
                self.map
                    .insert(outcome.clone(), outcome_probability * probability);
            }
        }
    }

    pub fn add_outcome(&mut self, outcome: SkillCheckOutcome, probability: Probability) {
        let entry = self.map.entry(outcome).or_insert(Probability::ZERO);
        *entry = (*entry).saturating_add(probability);
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
    use crate::check::modifier::Modifier;
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
            remaining_modifiers: ModifierState::default(),
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
            remaining_modifiers: ModifierState::default(),
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

    fn outcome_1() -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: Failure,
            remaining_modifiers: ModifierState::default(),
        }
    }

    fn outcome_2() -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: Success(QualityLevel::ONE),
            remaining_modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        }
    }

    fn outcome_3() -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: CriticalSuccess(QualityLevel::TWO),
            remaining_modifiers: ModifierState::default(),
        }
    }

    #[test]
    fn skill_check_outcome_probabilities_mul_works_for_non_empty_map() {
        let skill_check_probabilities = SkillCheckOutcomeProbabilities::from([
            (outcome_1(), prob(0.2)),
            (outcome_2(), prob(0.4)),
        ]);

        let actual = skill_check_probabilities * prob(0.5);
        let epsilon = 0.001;

        assert_that!(&actual.map).has_length(2);
        assert_that!(actual.probability_of_outcome(&outcome_1())).is_close_to(prob(0.1), epsilon);
        assert_that!(actual.probability_of_outcome(&outcome_2())).is_close_to(prob(0.2), epsilon);
    }

    #[rstest]
    #[case::both_empty([], [], [])]
    #[case::lhs_empty([], [(outcome_1(), prob(0.5))], [(outcome_1(), prob(0.5))])]
    #[case::rhs_empty([(outcome_1(), prob(0.5))], [], [(outcome_1(), prob(0.5))])]
    #[case::disjunctive(
        [(outcome_1(), prob(0.2))],
        [(outcome_2(), prob(0.3)), (outcome_3(), prob(0.5))],
        [(outcome_1(), prob(0.2)), (outcome_2(), prob(0.3)), (outcome_3(), prob(0.5))]
    )]
    #[case::overlap_without_saturation(
        [(outcome_1(), prob(0.3))],
        [(outcome_1(), prob(0.4)), (outcome_2(), prob(0.1))],
        [(outcome_1(), prob(0.7)), (outcome_2(), prob(0.1))]
    )]
    #[case::overlap_with_saturation(
        [(outcome_1(), prob(0.5)), (outcome_2(), prob(0.6))],
        [(outcome_1(), prob(0.5)), (outcome_2(), prob(0.5))],
        [(outcome_1(), prob(1.0)), (outcome_2(), prob(1.0))]
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

    #[rstest]
    #[case::both_empty([], [], 1.0, [])]
    #[case::lhs_empty([], [(outcome_1(), prob(0.5))], 0.4, [(outcome_1(), prob(0.2))])]
    #[case::rhs_empty([(outcome_1(), prob(0.5))], [], 0.4, [(outcome_1(), prob(0.5))])]
    #[case::disjunctive(
        [(outcome_1(), prob(0.1))],
        [(outcome_2(), prob(0.2))],
        0.5,
        [(outcome_1(), prob(0.1)), (outcome_2(), prob(0.1))]
    )]
    #[case::overlap_without_saturation(
        [(outcome_1(), prob(0.6))],
        [(outcome_1(), prob(0.6)), (outcome_2(), prob(0.2))],
        0.5,
        [(outcome_1(), prob(0.9)), (outcome_2(), prob(0.1))]
    )]
    #[case::overlap_with_saturation(
        [(outcome_1(), prob(0.3)), (outcome_2(), prob(0.5))],
        [(outcome_2(), prob(1.0))],
        0.6,
        [(outcome_1(), prob(0.3)), (outcome_2(), prob(1.0))]
    )]
    fn skill_check_outcome_probabilities_saturating_fma_assign_works(
        #[case] lhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] rhs: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] probability: f64,
        #[case] expected: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
    ) {
        let mut lhs = SkillCheckOutcomeProbabilities::from(lhs);
        let rhs = SkillCheckOutcomeProbabilities::from(rhs);
        let probability = Probability::new(probability).unwrap();
        let expected = SkillCheckOutcomeProbabilities::from(expected);

        lhs.saturating_fma_assign(&rhs, probability);
        let epsilon = 0.001;

        assert_that!(lhs).is_close_to(expected, epsilon);
    }
}
