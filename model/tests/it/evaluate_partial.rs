use SkillCheckOutcomeKind::{CriticalFailure, Failure, SpectacularFailure};
use kernal::prelude::*;
use model::check::PartialSkillCheckState;
use model::check::modifier::ModifierState;
use model::check::outcome::SkillCheckOutcomeKind::{CriticalSuccess, SpectacularSuccess, Success};
use model::check::outcome::{SkillCheckOutcomeKind, SkillCheckOutcomeProbabilities};
use model::roll::Roll;
use model::skill::{Attribute, SkillPoints};
use scholle::error::{RuntimeError, RuntimeErrorKind};

use crate::*;

fn default() -> PartialSkillCheckState {
    PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        roll_caps: [None, None, None],
        fixed_rolls: [None, None, None],
        skill_value: SkillPoints::new(0),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::default(),
        inaptitude: false,
    }
}

#[test]
fn simple_call_with_zero_skill_points() {
    let success_eval = eval(1.0);
    let critical_success_eval = eval(1_000.0);
    let spectacular_success_eval = eval(1_000_000.0);
    let engine = engine(format!(
        "if is_spectacular_success then {} else \
            if is_critical_success then {} else \
            if is_success then {} else 0",
        spectacular_success_eval.as_f64(),
        critical_success_eval.as_f64(),
        success_eval.as_f64()
    ));

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
            ..default()
        })
        .unwrap();

    let success_probability = (1000.0 - 28.0) / 8000.0;
    let critical_success_probability = 57.0 / 8000.0;
    let spectacular_success_probability = 1.0 / 8000.0;
    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob(SpectacularFailure, 1.0 / 8000.0),
        outcome_prob(CriticalFailure, 57.0 / 8000.0),
        outcome_prob(Failure, (7000.0 - 88.0) / 8000.0),
        outcome_prob(Success(QL_1), success_probability),
        outcome_prob(CriticalSuccess(QL_1), critical_success_probability),
        outcome_prob(SpectacularSuccess(QL_1), spectacular_success_probability),
    ]);
    let expected_evaluation = success_eval * prob(success_probability)
        + critical_success_eval * prob(critical_success_probability)
        + spectacular_success_eval * prob(spectacular_success_probability);

    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
}

#[test]
fn inaptitude_with_zero_skill_points() {
    // Attribute values 10, 10, 10 Skill value 0
    // Spectacular Success:
    //   requires 4x 1 (20^-4 = 1 / 160k)
    // Critical Success:
    //   requires 3x 1 and 1x non-1 (4 * 19 / 160k = 76 / 160k)
    // Success:
    //   requires 4x <=10 and not 3x 1 and 1x 2-10
    //   2^-4 - (4 * 9 + 1) / 160k = 10000 / 160k - 37 / 160k = 9963 / 160k
    // Spectacular Failure:
    //   requires 4x20 or 2x 20 and 1x non-20 and then 1x 20 ((3 * 19 + 1) / 160k = 58 / 160k)
    // Critical Failure:
    //   requires 2x 20 and 2x non-20 or 3x 20 and then 1x non-20
    //   (19 + 6 * 19 * 19) / 160k = 2185 / 160k
    // Failure
    //   the rest: (160k - 1 - 76 - 9963 - 77 - 2166) / 160k = 147717 / 160k

    let engine = engine("quality_level");

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
            inaptitude: true,
            ..default()
        })
        .unwrap();

    let success_probability = 9_963.0 / 160_000.0;
    let critical_success_probability = 76.0 / 160_000.0;
    let spectacular_success_probability = 1.0 / 160_000.0;
    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob(SpectacularFailure, 58.0 / 160_000.0),
        outcome_prob(CriticalFailure, 2_185.0 / 160_000.0),
        outcome_prob(Failure, 147_717.0 / 160_000.0),
        outcome_prob(Success(QL_1), success_probability),
        outcome_prob(CriticalSuccess(QL_1), critical_success_probability),
        outcome_prob(SpectacularSuccess(QL_1), spectacular_success_probability),
    ]);
    let expected_evaluation =
        eval(success_probability + critical_success_probability + spectacular_success_probability);

    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn evaluation_error() {
    // Fails at QL 4
    let engine = engine("100 / (4 - quality_level)");
    let state = PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        roll_caps: [None, None, None],
        fixed_rolls: [None, None, None],
        skill_value: SkillPoints::new(18),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::default(),
        inaptitude: false,
    };

    let result = engine.evaluate_partial(state);

    assert_that!(result).contains_error(RuntimeError {
        kind: RuntimeErrorKind::DivideByZero,
        span: (0..25).into(),
    });
}

#[test]
fn evaluation_error_with_cap() {
    let engine = engine("1.0 / as_float(4 - quality_level)");
    let state = PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        roll_caps: [None, Some(Roll::new(10).unwrap()), None],
        fixed_rolls: [
            Some(Roll::new(10).unwrap()),
            None,
            Some(Roll::new(10).unwrap()),
        ],
        skill_value: SkillPoints::new(10),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::default(),
        inaptitude: false,
    };

    let result = engine.evaluate_partial(state);

    assert_that!(result).contains_error(RuntimeError {
        kind: RuntimeErrorKind::InvalidResult(f64::INFINITY),
        span: (0..33).into(),
    });
}
