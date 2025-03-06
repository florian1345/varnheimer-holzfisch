use crate::check::SkillCheckProbabilities;
use crate::probability::Probability;
use crate::skill::{QualityLevel, QualityLevelMap, QUALITY_LEVEL_COUNT};

use kernal::abs_diff::AbsDiff;
use kernal::prelude::*;
use crate::evaluation::Evaluation;

pub fn create_quality_level_map<T: Copy + Default, const LEN: usize>(
    values: [T; LEN]
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

impl AbsDiff for SkillCheckProbabilities {
    type ReturnType = f64;

    fn abs_diff(&self, other: &SkillCheckProbabilities) -> f64 {
        self.spectacular_failure_probability.abs_diff(&other.spectacular_failure_probability)
            .max(self.critical_failure_probability.abs_diff(&other.critical_failure_probability))
            .max(self.failure_probability.abs_diff(&other.failure_probability))
            .max(self.success_probabilities_by_quality_level
                .abs_diff(&other.success_probabilities_by_quality_level))
            .max(self.critical_success_probabilities_by_quality_level
                .abs_diff(&other.critical_success_probabilities_by_quality_level))
            .max(self.spectacular_success_probabilities_by_quality_level
                .abs_diff(&other.spectacular_success_probabilities_by_quality_level))
    }
}
