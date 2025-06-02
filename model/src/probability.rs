use std::ops::{Add, AddAssign, Mul, MulAssign};

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
pub struct Probability(f64);

impl Probability {
    pub const ONE: Probability = Probability(1.0);
    pub const ZERO: Probability = Probability(0.0);

    pub const fn new(prob: f64) -> Option<Probability> {
        if prob.is_nan() || prob > 1.0 || prob < 0.0 {
            return None;
        }

        Some(Probability(prob))
    }

    pub fn saturating_add(self, other: Probability) -> Probability {
        Probability((self.0 + other.0).min(1.0))
    }

    pub fn checked_add(self, other: Probability) -> Option<Probability> {
        Probability::new(self.0 + other.0)
    }

    pub fn saturating_mul(self, factor: usize) -> Probability {
        Probability((self.0 * factor as f64).min(1.0))
    }

    pub fn as_f64(self) -> f64 {
        self.0
    }
}

impl AddAssign for Probability {
    fn add_assign(&mut self, other: Probability) {
        *self = *self + other;
    }
}

impl Add for Probability {
    type Output = Probability;

    fn add(self, other: Probability) -> Probability {
        self.checked_add(other).unwrap()
    }
}

impl MulAssign for Probability {
    fn mul_assign(&mut self, rhs: Probability) {
        self.0 *= rhs.0;
    }
}

impl Mul for Probability {
    type Output = Probability;

    fn mul(mut self, rhs: Probability) -> Probability {
        self *= rhs;
        self
    }
}

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;
    use rstest_reuse::{apply, template};

    use super::*;

    fn prob(value: f64) -> Probability {
        Probability::new(value).unwrap()
    }

    #[rstest]
    #[case::neg_infinity(f64::NEG_INFINITY)]
    #[case::negative(-0.001)]
    #[case::too_large(1.001)]
    #[case::infinity(f64::INFINITY)]
    #[case::nan(f64::NAN)]
    fn new_probability_outside_legal_range_is_none(#[case] value: f64) {
        assert_that!(Probability::new(value)).is_none();
    }

    #[rstest]
    #[case::min(0.0)]
    #[case::max(1.0)]
    #[case::inside_range(0.3)]
    fn new_probability_inside_legal_range_is_some(#[case] value: f64) {
        let probability = Probability::new(value);

        assert_that!(probability).is_some();
        assert_that!(probability.unwrap().as_f64()).is_equal_to(value);
    }

    #[template]
    #[rstest]
    #[case::zeros(0.0, 0.0, 0.0)]
    #[case::zero_and_non_zero(0.0, 0.8, 0.8)]
    #[case::non_zero_and_zero(0.2, 0.0, 0.2)]
    #[case::both_non_zero(0.49, 0.5, 0.99)]
    fn additions_in_bounds(#[case] lhs: f64, #[case] rhs: f64, #[case] expected: f64) {}

    #[template]
    #[rstest]
    #[case::one_plus_epsilon(1.0, 0.01)]
    #[case::epsilon_plus_one(0.01, 1.0)]
    #[case::one_plus_one(1.0, 1.0)]
    #[case::greater_half_plus_half(0.51, 0.5)]
    fn additions_out_of_bounds(#[case] lhs: f64, #[case] rhs: f64) {}

    #[apply(additions_in_bounds)]
    fn saturating_add_within_bounds_returns_sum(
        #[case] lhs: f64,
        #[case] rhs: f64,
        #[case] expected: f64,
    ) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);

        assert_that!(lhs.saturating_add(rhs).as_f64()).is_close_to(expected, 0.001);
    }

    #[apply(additions_out_of_bounds)]
    fn saturating_add_crossing_upper_bound_is_one(#[case] lhs: f64, #[case] rhs: f64) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);

        assert_that!(lhs.saturating_add(rhs).as_f64()).is_close_to(1.0, 0.001);
    }

    #[apply(additions_in_bounds)]
    fn checked_add_within_bounds_returns_sum(
        #[case] lhs: f64,
        #[case] rhs: f64,
        #[case] expected: f64,
    ) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);
        let sum = lhs.checked_add(rhs);

        assert_that!(sum).is_some();
        assert_that!(sum.unwrap().as_f64()).is_close_to(expected, 0.001);
    }

    #[apply(additions_out_of_bounds)]
    fn checked_add_crossing_upper_bound_is_none(#[case] lhs: f64, #[case] rhs: f64) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);

        assert_that!(lhs.checked_add(rhs)).is_none();
    }

    #[apply(additions_in_bounds)]
    fn add_within_bounds_returns_sum(#[case] lhs: f64, #[case] rhs: f64, #[case] expected: f64) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);

        assert_that!((lhs + rhs).as_f64()).is_close_to(expected, 0.001);
    }

    #[apply(additions_out_of_bounds)]
    fn add_crossing_upper_bound_panics(#[case] lhs: f64, #[case] rhs: f64) {
        let lhs = prob(lhs);
        let rhs = prob(rhs);

        assert_that!(|| lhs + rhs).panics();
    }

    #[test]
    fn mul_works() {
        let lhs = prob(0.5);
        let rhs = prob(0.4);

        assert_that!((lhs * rhs).as_f64()).is_close_to(0.2, 0.001);
    }

    #[rstest]
    #[case(1.0, 0, 0.0)]
    #[case(1.0, 1, 1.0)]
    #[case(1.0, 2, 1.0)]
    #[case(0.3, 2, 0.6)]
    #[case(0.3, 3, 0.9)]
    #[case(0.3, 4, 1.0)]
    #[case(0.0, 4, 0.0)]
    fn saturating_mul_with_usize_works(
        #[case] lhs: f64,
        #[case] rhs: usize,
        #[case] expected: f64,
    ) {
        assert_that!(prob(lhs).saturating_mul(rhs)).is_close_to(prob(expected), 0.001);
    }
}
