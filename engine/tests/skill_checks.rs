use std::collections::HashMap;

use engine::HolzfischEngine;

use model::check::{PartialSkillCheckState, SkillCheckOutcome, SkillCheckProbabilities};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluation, SkillCheckEvaluator};
use model::skill::{Attribute, QualityLevel, SkillPoints};

use kernal::prelude::*;
use model::probability::Probability;
use model::test_util::create_quality_level_map;

struct PerOutcomeEvaluator {
    outcomes: HashMap<SkillCheckOutcome, Evaluation>
}

impl PerOutcomeEvaluator {
    fn new(
        outcomes: impl IntoIterator<Item = (SkillCheckOutcome, Evaluation)>
    ) -> PerOutcomeEvaluator {
        PerOutcomeEvaluator {
            outcomes: outcomes.into_iter()
                .map(|(outcome, evaluation)| (outcome, evaluation))
                .collect()
        }
    }
}

impl SkillCheckEvaluator for PerOutcomeEvaluator {
    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation {
        self.outcomes.get(&outcome).cloned().unwrap_or(Evaluation::ZERO)
    }
}

fn prob(value: f64) -> Probability {
    Probability::new(value).unwrap()
}

const EPS: f64 = 1e-6;

#[test]
fn simple_call_with_zero_skill_points() {
    let success_eval = Evaluation::new(1.0).unwrap();
    let critical_success_eval = Evaluation::new(1_000.0).unwrap();
    let spectacular_success_eval = Evaluation::new(1_000_000.0).unwrap();
    let mut engine = HolzfischEngine {
        evaluator: PerOutcomeEvaluator::new([
            (SkillCheckOutcome::Success(QualityLevel::ONE), success_eval),
            (SkillCheckOutcome::CriticalSuccess(QualityLevel::ONE), critical_success_eval),
            (SkillCheckOutcome::SpectacularSuccess(QualityLevel::ONE), spectacular_success_eval)
        ])
    };

    let evaluated = engine.evaluate_partial(PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        fixed_rolls: [None, None, None],
        skill_value: SkillPoints::new(0),
    });

    let success_probability = prob((1000.0 - 28.0) / 8000.0);
    let critical_success_probability = prob(57.0 / 8000.0);
    let spectacular_success_probability = prob(1.0 / 8000.0);
    let expected_probs = SkillCheckProbabilities {
        spectacular_failure_probability: prob(1.0 / 8000.0),
        critical_failure_probability: prob(57.0 / 8000.0),
        failure_probability: prob((7000.0 - 88.0) / 8000.0),
        success_probabilities_by_quality_level:
            create_quality_level_map([success_probability]),
        critical_success_probabilities_by_quality_level:
            create_quality_level_map([critical_success_probability]),
        spectacular_success_probabilities_by_quality_level:
            create_quality_level_map([spectacular_success_probability])
    };
    let expected_evaluation =
        success_eval * success_probability
            + critical_success_eval * critical_success_probability
            + spectacular_success_eval * spectacular_success_probability;

    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}
