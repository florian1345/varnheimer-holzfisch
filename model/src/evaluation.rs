use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg};

use crate::check::outcome::{SkillCheckOutcome, SkillCheckOutcomeProbabilities};
use crate::probability::Probability;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Evaluation(f64);

impl Evaluation {
    pub const ZERO: Evaluation = Evaluation(0.0);

    pub const fn new(value: f64) -> Option<Evaluation> {
        if value.is_finite() {
            Some(Evaluation(value))
        }
        else {
            None
        }
    }

    pub fn as_f64(self) -> f64 {
        self.0
    }
}

impl Eq for Evaluation {}

impl Ord for Evaluation {
    fn cmp(&self, other: &Evaluation) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl PartialOrd for Evaluation {
    fn partial_cmp(&self, other: &Evaluation) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// TODO this is not sound - adding evaluations reach infinity or -infinity
//  even worse, infinity + (-infinity) is NaN
//  we may want a checked_add (or explicitly call this unchecked_add)

impl AddAssign for Evaluation {
    fn add_assign(&mut self, rhs: Evaluation) {
        self.0 += rhs.0;
    }
}

impl Add for Evaluation {
    type Output = Evaluation;

    fn add(mut self, rhs: Evaluation) -> Evaluation {
        self += rhs;
        self
    }
}

impl MulAssign<Probability> for Evaluation {
    fn mul_assign(&mut self, rhs: Probability) {
        self.0 *= rhs.as_f64();
    }
}

impl Mul<Probability> for Evaluation {
    type Output = Evaluation;

    fn mul(mut self, rhs: Probability) -> Evaluation {
        self *= rhs;
        self
    }
}

impl Neg for Evaluation {
    type Output = Evaluation;

    fn neg(self) -> Evaluation {
        Evaluation(-self.0)
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Evaluated<T> {
    pub evaluated: T,
    pub evaluation: Evaluation,
}

impl<T> Evaluated<T> {
    pub fn compare_evaluation(&self, other: &Evaluated<T>) -> Ordering {
        self.evaluation.cmp(&other.evaluation)
    }
}

pub trait SkillCheckEvaluator {
    type Error;

    fn evaluate(&self, outcome: &SkillCheckOutcome) -> Result<Evaluation, Self::Error>;

    fn evaluate_probabilities(
        &self,
        probabilities: &SkillCheckOutcomeProbabilities,
    ) -> Result<Evaluation, Self::Error> {
        let mut evaluation = Evaluation::ZERO;

        for (outcome, probability) in probabilities.outcomes() {
            evaluation += self.evaluate(outcome)? * probability;
        }

        Ok(evaluation)
    }
}

#[cfg(test)]
mod tests {
    use std::convert::Infallible;

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;
    use crate::check::outcome::SkillCheckOutcomeKind;

    #[rstest]
    #[case::neg_infinity(f64::NEG_INFINITY)]
    #[case::infininty(f64::INFINITY)]
    #[case::nan(f64::NAN)]
    fn new_evaluation_with_non_finite_is_none(#[case] value: f64) {
        let evaluation = Evaluation::new(value);

        assert_that!(evaluation).is_none();
    }

    #[rstest]
    #[case::max(f64::MIN)]
    #[case::negative(-123.456)]
    #[case::zero(0.0)]
    #[case::positive(123.456)]
    #[case::max(f64::MAX)]
    fn new_evaluation_with_non_nan_is_some(#[case] value: f64) {
        let evaluation = Evaluation::new(value);

        assert_that!(evaluation).is_some();
        assert_that!(evaluation.unwrap().as_f64()).is_equal_to(value);
    }

    #[test]
    fn adding_evaluations_works() {
        let lhs = Evaluation::new(1.0).unwrap();
        let rhs = Evaluation::new(2.0).unwrap();

        assert_that!((lhs + rhs).as_f64()).is_close_to(3.0, 0.001);
    }

    #[test]
    fn multiplying_evaluations_works() {
        let lhs = Evaluation::new(2.0).unwrap();
        let rhs = Probability::new(0.3).unwrap();

        assert_that!((lhs * rhs).as_f64()).is_close_to(0.6, 0.001);
    }

    #[test]
    fn comparing_evaluations_works() {
        let smaller = Evaluation::new(1.0).unwrap();
        let larger = Evaluation::new(2.0).unwrap();

        assert_that!(smaller).is_less_than(larger);
        assert_that!(larger).is_greater_than(smaller);
        assert_that!(smaller.cmp(&smaller)).is_equal_to(Ordering::Equal);
    }

    #[test]
    fn comparing_evaluated_works() {
        let smaller = Evaluated {
            evaluated: (),
            evaluation: Evaluation::new(1.0).unwrap(),
        };
        let larger = Evaluated {
            evaluated: (),
            evaluation: Evaluation::new(2.0).unwrap(),
        };

        assert_that!(smaller.compare_evaluation(&larger)).is_equal_to(Ordering::Less);
        assert_that!(larger.compare_evaluation(&smaller)).is_equal_to(Ordering::Greater);
        assert_that!(smaller.compare_evaluation(&smaller)).is_equal_to(Ordering::Equal);
    }

    struct TestOutcome {
        outcome: SkillCheckOutcome,
        evaluation: Evaluation,
        probability: Probability,
    }

    impl TestOutcome {
        fn new(kind: SkillCheckOutcomeKind, evaluation: f64, probability: f64) -> TestOutcome {
            TestOutcome {
                outcome: SkillCheckOutcome {
                    kind,
                    remaining_modifiers: Default::default(),
                },
                evaluation: Evaluation::new(evaluation).unwrap(),
                probability: Probability::new(probability).unwrap(),
            }
        }
    }

    #[rstest]
    #[case([], 0.0)]
    #[case([TestOutcome::new(SkillCheckOutcomeKind::Failure, 0.5, 0.8)], 0.4)]
    #[case(
        [
            TestOutcome::new(SkillCheckOutcomeKind::SpectacularFailure, 0.2, 0.5),
            TestOutcome::new(SkillCheckOutcomeKind::CriticalFailure, 0.3, 0.3),
            TestOutcome::new(SkillCheckOutcomeKind::Failure, 0.4, 0.2)
        ],
        0.27
    )]
    fn evaluate_probabilities_works(
        #[case] outcomes: impl IntoIterator<Item = TestOutcome>,
        #[case] expected: f64,
    ) {
        struct TestEvaluator<'outcomes> {
            outcomes: &'outcomes [TestOutcome],
        }

        impl<'outcomes> SkillCheckEvaluator for TestEvaluator<'outcomes> {
            type Error = Infallible;

            fn evaluate(&self, outcome: &SkillCheckOutcome) -> Result<Evaluation, Infallible> {
                Ok(self
                    .outcomes
                    .iter()
                    .find(|test_outcome| &test_outcome.outcome == outcome)
                    .unwrap()
                    .evaluation)
            }
        }

        let outcomes = outcomes.into_iter().collect::<Vec<_>>();
        let evaluator = TestEvaluator {
            outcomes: &outcomes,
        };
        let probabilities = SkillCheckOutcomeProbabilities::from(
            outcomes
                .iter()
                .map(|test_outcome| (test_outcome.outcome.clone(), test_outcome.probability)),
        );

        let evaluation = evaluator.evaluate_probabilities(&probabilities).unwrap();

        assert_that!(evaluation.as_f64()).is_close_to(expected, 0.001);
    }
}
