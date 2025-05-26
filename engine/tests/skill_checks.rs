use std::collections::HashMap;

use engine::HolzfischEngine;

use model::check::{
    PartialSkillCheckState,
    Reroll,
    SkillCheckAction,
    SkillCheckOutcome,
    SkillCheckProbabilities,
    SkillCheckState
};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluation, SkillCheckEvaluator};
use model::skill::{Attribute, QualityLevel, SkillPoints};

use kernal::prelude::*;
use model::probability::Probability;
use model::roll::Roll;
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

struct QualityLevelEvaluator;

impl SkillCheckEvaluator for QualityLevelEvaluator {
    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation {
        outcome.quality_level()
            .map(|ql| Evaluation::new(ql.as_u8() as f64).unwrap())
            .unwrap_or(Evaluation::ZERO)
    }
}

fn prob(value: f64) -> Probability {
    Probability::new(value).unwrap()
}

fn eval(value: f64) -> Evaluation {
    Evaluation::new(value).unwrap()
}

const EPS: f64 = 1e-6;

#[test]
fn simple_call_with_zero_skill_points() {
    let success_eval = eval(1.0);
    let critical_success_eval = eval(1_000.0);
    let spectacular_success_eval = eval(1_000_000.0);
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
        fate_points: 0,
        quality_level_increase: None,
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

#[test]
fn given_roll_evaluates_options_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(11), Attribute::new(10)],
        rolls: [Roll::new(16).unwrap(), Roll::new(4).unwrap(), Roll::new(10).unwrap()],
        skill_value: SkillPoints::new(7),
        fate_points: 1,
        quality_level_increase: None,
    };

    let mut engine = HolzfischEngine {
        evaluator: QualityLevelEvaluator,
    };

    let evaluated = engine.evaluate_all_actions(skill_check_state);

    // The best option is to reroll the 18, which yields the following QL distribution:
    // Failure: 5% (20 on reroll)
    // QL1: 20% (19-16 on reroll)
    // QL2: 15% (15-13 on reroll)
    // QL3: 60% (12-1 on reroll)
    // Evaluation: 0,2 * 1 + 0,15 * 2 + 0,6 * 3 = 2,3
    // Increasing QL yields a 100% probability of QL2, so evaluation 2.
    // Everything else should be worse.

    let best_move = evaluated[0];
    let second_best_move = evaluated[1];

    assert_that!(evaluated).has_length(9); // accept + increase QL + 7 reroll combos
    assert_that!(best_move.0)
        .is_equal_to(SkillCheckAction::Reroll(Reroll::new([true, false, false]).unwrap()));
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.3), EPS);
    assert_that!(second_best_move.0).is_equal_to(SkillCheckAction::IncreaseQualityLevel);
    assert_that!(second_best_move.1.evaluation).is_close_to(eval(2.0), EPS);
}
