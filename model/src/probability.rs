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
