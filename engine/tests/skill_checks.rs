use std::collections::HashMap;

use engine::HolzfischEngine;

use model::check::{PartialSkillCheckState, Reroll, SkillCheckAction, SkillCheckOutcome, SkillCheckOutcomeKind, SkillCheckOutcomeProbabilities, SkillCheckState};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluation, SkillCheckEvaluator};
use model::skill::{Attribute, QualityLevel, SkillPoints};

use kernal::prelude::*;
use model::probability::Probability;
use model::roll::Roll;

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

#[derive(Default)]
struct QualityLevelEvaluator {
    fate_point_value: Evaluation
}

impl SkillCheckEvaluator for QualityLevelEvaluator {
    fn evaluate(&mut self, outcome: SkillCheckOutcome) -> Evaluation {
        let quality_level_value = outcome.quality_level()
            .map(|ql| Evaluation::new(ql.as_u8() as f64).unwrap())
            .unwrap_or(Evaluation::ZERO);
        let fate_point_value = self.fate_point_value * outcome.remaining_fate_points;

        quality_level_value + fate_point_value
    }
}

fn prob(value: f64) -> Probability {
    Probability::new(value).unwrap()
}

fn eval(value: f64) -> Evaluation {
    Evaluation::new(value).unwrap()
}

fn outcome_no_fate_points(kind: SkillCheckOutcomeKind) -> SkillCheckOutcome {
    SkillCheckOutcome {
        kind,
        remaining_fate_points: 0
    }
}

const EPS: f64 = 1e-6;

#[test]
fn simple_call_with_zero_skill_points() {
    let success_eval = eval(1.0);
    let critical_success_eval = eval(1_000.0);
    let spectacular_success_eval = eval(1_000_000.0);
    let success =
        outcome_no_fate_points(SkillCheckOutcomeKind::Success(QualityLevel::ONE));
    let critical_success =
        outcome_no_fate_points(SkillCheckOutcomeKind::CriticalSuccess(QualityLevel::ONE));
    let spectacular_success =
        outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularSuccess(QualityLevel::ONE));
    let mut engine = HolzfischEngine {
        evaluator: PerOutcomeEvaluator::new([
            (success, success_eval),
            (critical_success, critical_success_eval),
            (spectacular_success, spectacular_success_eval)
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
    let expected_probs = SkillCheckOutcomeProbabilities::from([
        (outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularFailure), prob(1.0 / 8000.0)),
        (outcome_no_fate_points(SkillCheckOutcomeKind::CriticalFailure), prob(57.0 / 8000.0)),
        (outcome_no_fate_points(SkillCheckOutcomeKind::Failure), prob((7000.0 - 88.0) / 8000.0)),
        (success, success_probability),
        (critical_success, critical_success_probability),
        (spectacular_success, spectacular_success_probability),
    ]);
    let expected_evaluation =
        success_eval * success_probability
            + critical_success_eval * critical_success_probability
            + spectacular_success_eval * spectacular_success_probability;

    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn given_roll_with_fate_point_evaluates_options_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(11), Attribute::new(10)],
        rolls: [Roll::new(16).unwrap(), Roll::new(4).unwrap(), Roll::new(10).unwrap()],
        skill_value: SkillPoints::new(7),
        fate_points: 1,
        quality_level_increase: None,
    };

    let mut engine = HolzfischEngine {
        evaluator: QualityLevelEvaluator::default(),
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

    let best_move = evaluated[0].clone();
    let second_best_move = evaluated[1].clone();

    assert_that!(evaluated).has_length(9); // accept + increase QL + 7 reroll combos
    assert_that!(best_move.0)
        .is_equal_to(SkillCheckAction::Reroll(Reroll::new([true, false, false]).unwrap()));
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.3), EPS);
    assert_that!(second_best_move.0).is_equal_to(SkillCheckAction::IncreaseQualityLevel);
    assert_that!(second_best_move.1.evaluation).is_close_to(eval(2.0), EPS);
}

#[test]
fn given_roll_with_fate_point_evaluates_cost_of_fate_point_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(9), Attribute::new(13), Attribute::new(11)],
        rolls: [Roll::new(13).unwrap(), Roll::new(10).unwrap(), Roll::new(12).unwrap()],
        skill_value: SkillPoints::new(8),
        fate_points: 1,
        quality_level_increase: None,
    };

    let mut engine = HolzfischEngine {
        evaluator: QualityLevelEvaluator {
            fate_point_value: eval(1.5)
        },
    };

    let evaluated = engine.evaluate_all_actions(skill_check_state);

    // None of the options yields an average increase of >1.5 QL, hence accepting is best.
    // The value is the achieved QL (1) + the value of remaining fate points (1.5).

    let best_move = evaluated[0].clone();

    assert_that!(evaluated).has_length(9); // accept + increase QL + 7 reroll combos
    assert_that!(best_move.0).is_equal_to(SkillCheckAction::Accept);
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.5), EPS);
}
