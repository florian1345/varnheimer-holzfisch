use crate::probability::Probability;
use crate::skill::{QualityLevel, QualityLevelMap, QUALITY_LEVEL_COUNT};

use kernal::abs_diff::AbsDiff;

pub(crate) fn create_quality_level_map<T: Copy + Default>(
    values: [T; QUALITY_LEVEL_COUNT]
) -> QualityLevelMap<T> {
    let mut map = QualityLevelMap::<T>::default();

    map[QualityLevel::ONE] = values[0];
    map[QualityLevel::TWO] = values[1];
    map[QualityLevel::THREE] = values[2];
    map[QualityLevel::FOUR] = values[3];
    map[QualityLevel::FIVE] = values[4];
    map[QualityLevel::SIX] = values[5];

    map
}

impl AbsDiff for Probability {
    type ReturnType = f64;

    fn abs_diff(&self, other: &Probability) -> f64 {
        self.as_f64().abs_diff(&other.as_f64())
    }
}
