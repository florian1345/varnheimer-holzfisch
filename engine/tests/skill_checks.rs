use std::collections::HashMap;

use engine::HolzfischEngine;

use model::check::{PartialSkillCheckState, SkillCheckOutcome};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluation, SkillCheckEvaluator};
use model::skill::{Attribute, QualityLevel, SkillPoints};

use kernal::prelude::*;

struct PerOutcomeEvaluator {
    outcomes: HashMap<SkillCheckOutcome, Evaluation>
}

impl PerOutcomeEvaluator {
    fn new(
        outcomes: impl IntoIterator<Item = (SkillCheckOutcome, f64)>
    ) -> PerOutcomeEvaluator {
        PerOutcomeEvaluator {
            outcomes: outcomes.into_iter()
                .map(|(outcome, evaluation)| (outcome, Evaluation::new(evaluation).unwrap()))
                .collect()
        }
    }
}

impl SkillCheckEvaluator for PerOutcomeEvaluator {
    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation {
        self.outcomes.get(&outcome).cloned().unwrap_or(Evaluation::ZERO)
    }
}

#[test]
fn simple_call_with_zero_skill_points() {
    let mut engine = HolzfischEngine {
        evaluator: PerOutcomeEvaluator::new([
            (SkillCheckOutcome::Success(QualityLevel::ONE), 1.0),
            (SkillCheckOutcome::CriticalSuccess(QualityLevel::ONE), 1_000.0),
            (SkillCheckOutcome::SpectacularSuccess(QualityLevel::ONE), 1_000_000.0)
        ])
    };

    let evaluated = engine.evaluate_partial(PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        fixed_rolls: [None, None, None],
        skill_value: SkillPoints::new(0),
    });

    let expected_spectacular_failure_probability = 1.0 / 8000.0;
    let expected_critical_failure_probability = 57.0 / 8000.0;
    let expected_failure_probability = (7000.0 - 88.0) / 8000.0;
    let expected_success_probability = (1000.0 - 28.0) / 8000.0;
    let expected_critical_success_probability = 57.0 / 8000.0;
    let expected_spectacular_success_probability = 1.0 / 8000.0;

    let epsilon = 0.001;

    assert_that!(evaluated.evaluation.as_f64()).is_close_to(
        expected_success_probability +
            expected_critical_success_probability * 1_000.0 +
            expected_spectacular_success_probability * 1_000_000.0, epsilon);

    let probs = evaluated.evaluated;

    assert_that!(probs.spectacular_failure_probability.as_f64())
        .is_close_to(expected_spectacular_failure_probability, epsilon);
    assert_that!(probs.critical_failure_probability.as_f64())
        .is_close_to(expected_critical_failure_probability, epsilon);
    assert_that!(probs.failure_probability.as_f64())
        .is_close_to(expected_failure_probability, epsilon);
    assert_that!(probs.success_probabilities_by_quality_level[QualityLevel::ONE].as_f64())
        .is_close_to(expected_success_probability, epsilon);
    assert_that!(probs.critical_success_probabilities_by_quality_level[QualityLevel::ONE].as_f64())
        .is_close_to(expected_critical_success_probability, epsilon);
    assert_that!(probs.spectacular_success_probabilities_by_quality_level[QualityLevel::ONE]
        .as_f64()).is_close_to(expected_spectacular_success_probability, epsilon);

    assert_that!(&QualityLevel::ALL[1..]).contains_only_elements_matching(|&ql|
        probs.success_probabilities_by_quality_level[ql].as_f64() == 0.0 &&
            probs.critical_success_probabilities_by_quality_level[ql].as_f64() == 0.0 &&
            probs.spectacular_success_probabilities_by_quality_level[ql].as_f64() == 0.0);
}
