use crate::check::SkillCheckOutcome;
use crate::probability::Probability;

use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
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
        self.partial_cmp(other).unwrap()
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Evaluated<T> {
    pub evaluated: T,
    pub evaluation: Evaluation
}

impl<T> Evaluated<T> {

    pub fn compare_evaluation(&self, other: &Evaluated<T>) -> Ordering {
        self.evaluation.cmp(&other.evaluation)
    }
}

pub trait SkillCheckEvaluator {

    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation;
}
