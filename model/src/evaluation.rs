use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg};

use crate::check::outcome::SkillCheckOutcome;
use crate::probability::Probability;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Evaluation(f64);

impl Evaluation {
    pub const ZERO: Evaluation = Evaluation(0.0);

    pub const fn new(value: f64) -> Option<Evaluation> {
        if value.is_nan() {
            None
        }
        else {
            Some(Evaluation(value))
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

impl MulAssign<usize> for Evaluation {
    fn mul_assign(&mut self, rhs: usize) {
        self.0 *= rhs as f64;
    }
}

impl Mul<usize> for Evaluation {
    type Output = Evaluation;

    fn mul(mut self, rhs: usize) -> Evaluation {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation;
}

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    #[test]
    fn new_evaluation_with_nan_is_none() {
        let evaluation = Evaluation::new(f64::NAN);

        assert_that!(evaluation).is_none();
    }

    #[rstest]
    #[case::neg_infinity(f64::NEG_INFINITY)]
    #[case::negative(-123.456)]
    #[case::zero(0.0)]
    #[case::positive(123.456)]
    #[case::infininty(f64::INFINITY)]
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
}
