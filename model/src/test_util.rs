use kernal::abs_diff::AbsDiff;
use kernal::prelude::*;

use crate::check::outcome::SkillCheckOutcomeProbabilities;
use crate::evaluation::Evaluation;
use crate::probability::Probability;
use crate::skill::{QUALITY_LEVEL_COUNT, QualityLevel, QualityLevelMap};

pub fn create_quality_level_map<T: Copy + Default, const LEN: usize>(
    values: [T; LEN],
) -> QualityLevelMap<T> {
    assert_that!(LEN).is_less_than_or_equal_to(QUALITY_LEVEL_COUNT);

    let mut map = QualityLevelMap::<T>::default();

    for (index, ql) in QualityLevel::ALL.iter().cloned().enumerate().take(LEN) {
        map[ql] = values[index];
    }

    map
}

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

impl AbsDiff for QualityLevelMap<Probability> {
    type ReturnType = f64;

    fn abs_diff(&self, other: &QualityLevelMap<Probability>) -> f64 {
        self.iter()
            .zip(other.iter())
            .map(|(a, b)| a.abs_diff(b))
            .reduce(f64::max)
            .unwrap()
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
