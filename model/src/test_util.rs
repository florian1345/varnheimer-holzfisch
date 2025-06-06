use kernal::abs_diff::AbsDiff;

use crate::check::outcome::SkillCheckOutcomeProbabilities;
use crate::evaluation::Evaluation;
use crate::probability::Probability;

impl AbsDiff for Evaluation {
    type ReturnType = f64;

    fn abs_diff(&self, other: &Evaluation) -> f64 {
        self.as_f64().abs_diff(&other.as_f64())
    }
}

impl AbsDiff for Probability {
    type ReturnType = f64;

    fn abs_diff(&self, other: &Probability) -> f64 {
        self.as_f64().abs_diff(&other.as_f64())
    }
}

impl AbsDiff for SkillCheckOutcomeProbabilities {
    type ReturnType = f64;

    fn abs_diff(&self, other: &SkillCheckOutcomeProbabilities) -> f64 {
        self.outcomes()
            .map(|(outcome, _)| outcome)
            .chain(other.outcomes().map(|(outcome, _)| outcome))
            .map(|outcome| {
                self.probability_of_outcome(outcome)
                    .abs_diff(&other.probability_of_outcome(outcome))
            })
            .reduce(f64::max)
            .unwrap_or(0.0)
    }
}
