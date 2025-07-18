use SkillCheckOutcomeKind::{CriticalFailure, Failure, SpectacularFailure};
use kernal::prelude::*;
use model::check::PartialSkillCheckState;
use model::check::modifier::{Modifier, ModifierState};
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
        outcome_prob_no_mod(SpectacularFailure, 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalFailure, 57.0 / 8000.0),
        outcome_prob_no_mod(Failure, (7000.0 - 88.0) / 8000.0),
        outcome_prob_no_mod(Success(QL_1), success_probability),
        outcome_prob_no_mod(CriticalSuccess(QL_1), critical_success_probability),
        outcome_prob_no_mod(SpectacularSuccess(QL_1), spectacular_success_probability),
    ]);
    let expected_evaluation = success_eval * prob(success_probability)
        + critical_success_eval * prob(critical_success_probability)
        + spectacular_success_eval * prob(spectacular_success_probability);

    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
}

#[test]
fn simple_call_with_skill_points() {
    // Spectacular Success: [1, 1, 1]
    // => 1 / 8000 Value 20 (20 / 8000)
    // Critical Success (QL 3): [1, 1, 2-10], [1, 2-12, 1], [2-14, 1, 1]
    // => 33 / 8000 Value 6 (198 / 8000)
    // Critical Success (QL 2): [1, 1, 11-13], [1, 13-15, 1], [15-17, 1, 1]
    // => 9 / 8000 Value 4 (36 / 8000)
    // Critical Success (QL 1): [1, 1, 14-20], [1, 16-20, 1], [18-20, 1, 1]
    // => 15 / 8000 Value 2 (30 / 8000)
    // Success (QL 3): [1-14, 1-12, 1-10] - Spectacular Success - Critical Success (QL 3)
    // => 1646 / 8000 Value 3 (4938 / 8000)
    // Success (QL 2):
    //   ([1-14, 1-12, 11-13], [1-14, 13, 1-12], [1-14, 14, 1-11], [1-14, 15, 1-10],
    //   [15, 1-12, 1-12], [15, 13, 1-11], [15, 14, 1-10],
    //   [16, 1-12, 1-11], [16, 13, 1-10],
    //   [17, 1-12, 1-10]) - Critical Success (QL 2)
    // => 1384 / 8000 Value 2 (2768 / 8000)
    // Success (QL 1):
    //   ([1-14, 1-12, 14-17], [1-14, 13, 13-16], [1-14, 14, 12-15], [1-14, 15, 11-14],
    //     [1-14, 16, 1-13], [1-14, 17, 1-12], [1-14, 18, 1-11], [1-14, 19, 1-10],
    //   [15, 1-12, 13-16], [15, 13, 12-15], [15, 14, 11-14], [15, 15, 1-13], [15, 16, 1-12],
    //     [15, 17, 1-11], [15, 18, 1-10],
    //   [16, 1-12, 12-15], [16, 13, 11-14], [16, 14, 1-13], [16, 15, 1-12], [16, 16, 1-11],
    //     [16, 17, 1-10],
    //   [17, 1-12, 11-14], [17, 13, 1-13], [17, 14, 1-12], [17, 15, 1-11], [17, 16, 1-10],
    //   [18, 1-12, 1-13], [18, 13, 1-12], [18, 14, 1-11], [18, 15, 1-10],
    //   [19, 1-12, 1-12], [19, 13, 1-11], [19, 14, 1-10],
    //   [20, 1-12, 1-11], [20, 13, 1-10]) - ([1, 1, 14-17], [1, 16-19, 1], [18-20, 1, 1])
    // => 2263 / 8000 Value 1 (2263 / 8000)
    // Spectacular Failure: [20, 20, 20]
    // => 1 / 8000 Value -9 (-9 / 8000)
    // Critical Failure: [1-19, 20, 20], [20, 1-19, 20], [20, 20, 1-19]
    // => 57 / 8000 Value -7 (-399 / 8000)
    // Failure: Everything else
    // => 2591 / 8000 Value -1 (-2591 / 8000)

    let engine = engine(
        r#"
        if is_spectacular_success then 20
        else if is_critical_success then quality_level * 2
        else if is_success then quality_level
        else if is_spectacular_failure then -9
        else if is_critical_failure then -7
        else -1
    "#,
    );

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(14), Attribute::new(12), Attribute::new(10)],
            skill_value: SkillPoints::new(7),
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(SpectacularFailure, 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalFailure, 57.0 / 8000.0),
        outcome_prob_no_mod(Failure, 2591.0 / 8000.0),
        outcome_prob_no_mod(SpectacularSuccess(QL_3), 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_3), 33.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_2), 9.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_1), 15.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_3), 1646.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_2), 1384.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_1), 2263.0 / 8000.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(eval(7254.0 / 8000.0), EPS);
}

#[test]
fn extra_quality_level() {
    // Probabilities are equal to `simple_call_with_skill_points`, with QL shifted up by 1.

    let engine = engine("quality_level");

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(14), Attribute::new(12), Attribute::new(10)],
            skill_value: SkillPoints::new(7),
            extra_quality_levels_on_success: Some(QL_1),
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(SpectacularFailure, 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalFailure, 57.0 / 8000.0),
        outcome_prob_no_mod(Failure, 2591.0 / 8000.0),
        outcome_prob_no_mod(SpectacularSuccess(QL_4), 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_4), 33.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_3), 9.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_2), 15.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_4), 1646.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_3), 1384.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_2), 2263.0 / 8000.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(eval(15455.0 / 8000.0), EPS);
}

#[test]
fn extra_skill_points_on_success() {
    // Spectacular Success: [1, 1, 1]
    // => 1 / 8000 Value 12 (12 / 8000)
    // Critical Success (QL 2): [1, 1, 2-12], [1, 2-13, 1], [2-13, 1, 1]
    // => 35 / 8000 Value 12 (420 / 8000)
    // Critical Success (QL 1): [1, 1, 13-20], [1, 14-20, 1], [14-20, 1, 1]
    // => 22 / 8000 Value 10 (220 / 8000)
    // Success (QL 2):
    //   [1-13, 1-13, 1-12] - Critical Success (QL 2) - Spectacular Success
    // => 1992 / 8000 Value 12 (23904 / 8000)
    // Success (QL 1):
    //   ([1-13, 1-13, 13-14], [1-13, 14, 1-13], [1-13, 15, 1-12],
    //   [14, 1-13, 1-13], [14, 14, 1-12],
    //   [15, 1-13, 1-12]) - ([1, 1, 13-14], [1, 14-15, 1], [14-15, 1, 1])
    // => 994 / 8000 Value 10 (9940 / 8000)
    // Spectacular Failure: [20, 20, 20]
    // => 1 / 8000 Value -5 (-5 / 8000)
    // Critical Failure: [1-19, 20, 20], [20, 1-19, 20], [20, 20, 1-19]
    // => 57 / 8000 Value -5 (-285 / 8000)
    // Failure: Everything else
    // => 4898 / 8000 Value -5 (-24490 / 8000)

    let engine = engine("if is_success then 60.0 / (7.0 - as_float(quality_level)) else -5.0");

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(13), Attribute::new(13), Attribute::new(12)],
            skill_value: SkillPoints::new(2),
            extra_skill_points_on_success: SkillPoints::new(2),
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(SpectacularFailure, 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalFailure, 57.0 / 8000.0),
        outcome_prob_no_mod(Failure, 4898.0 / 8000.0),
        outcome_prob_no_mod(SpectacularSuccess(QL_2), 1.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_2), 35.0 / 8000.0),
        outcome_prob_no_mod(CriticalSuccess(QL_1), 22.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_2), 1992.0 / 8000.0),
        outcome_prob_no_mod(Success(QL_1), 994.0 / 8000.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(eval(9716.0 / 8000.0), EPS);
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
        outcome_prob_no_mod(SpectacularFailure, 58.0 / 160_000.0),
        outcome_prob_no_mod(CriticalFailure, 2_185.0 / 160_000.0),
        outcome_prob_no_mod(Failure, 147_717.0 / 160_000.0),
        outcome_prob_no_mod(Success(QL_1), success_probability),
        outcome_prob_no_mod(CriticalSuccess(QL_1), critical_success_probability),
        outcome_prob_no_mod(SpectacularSuccess(QL_1), spectacular_success_probability),
    ]);
    let expected_evaluation =
        eval(success_probability + critical_success_probability + spectacular_success_probability);

    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn with_one_given_roll() {
    // Attributes 10 10 10 Skill value 11 Second die fixed 5
    // Critical Success (QL 4): 1 / 400 Value 50 (50 / 400)
    // Success (QL 4): [1-10, 5, 1-11], [11, 5, 1-10] - Critical Success (QL 4)
    // => 119 / 400 Value 25 (2975 / 400)
    // Success (QL 3): [1-10, 5, 12-14], [11, 5, 11-13], [12, 5, 1-12], [13, 5, 1-11], [14, 5, 1-10]
    // => 66 / 400 Value 16 (1056 / 400)
    // Success (QL 2):
    //   [1-10, 5, 15-17], [11, 5, 14-16], [12, 5, 13-15], [13, 5, 12-14], [14, 5, 11-13],
    //   [15, 5, 1-12], [16, 5, 1-11], [17, 5, 1-10]
    // => 75 / 400 Value 9 (675 / 400)
    // Success (QL 1):
    //   [1-10, 5, 18-20], [11, 5, 17-20], [12, 5, 16-19], [13, 5, 15-18], [14, 5, 14-17],
    //   [15, 5, 13-16], [16, 5, 12-15], [17, 5, 11-14], [18, 5, 1-13], [19, 5, 1-12], [20, 5, 1-11]
    // => 94 / 400 Value 4 (376 / 400)
    // Critical Failure: 1 / 400 Value -20 (-20 / 400)
    // Failure: 44 / 400 Value 0

    let engine = engine(
        r#"
        let
          square = (x: int) -> x * x
        in
            if is_critical_success then 50
            else if is_success then square(quality_level + 1)
            else if is_critical_failure then -20
            else 0
    "#,
    );

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
            skill_value: SkillPoints::new(11),
            fixed_rolls: [None, Some(roll(5)), None],
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(CriticalFailure, 1.0 / 400.0),
        outcome_prob_no_mod(Failure, 44.0 / 400.0),
        outcome_prob_no_mod(CriticalSuccess(QL_4), 1.0 / 400.0),
        outcome_prob_no_mod(Success(QL_4), 119.0 / 400.0),
        outcome_prob_no_mod(Success(QL_3), 66.0 / 400.0),
        outcome_prob_no_mod(Success(QL_2), 75.0 / 400.0),
        outcome_prob_no_mod(Success(QL_1), 94.0 / 400.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(eval(5112.0 / 400.0), EPS);
}

#[test]
fn with_given_rolls_constituting_critical_failure() {
    let engine = engine("quality_level");

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            fixed_rolls: [Some(roll(20)), Some(roll(20)), None],
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(CriticalFailure, 19.0 / 20.0),
        outcome_prob_no_mod(SpectacularFailure, 1.0 / 20.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn with_given_rolls_constituting_critical_success() {
    let engine = engine("quality_level");

    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            fixed_rolls: [None, Some(roll(1)), Some(roll(1))],
            skill_value: SkillPoints::new(3),
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob_no_mod(CriticalSuccess(QL_1), 19.0 / 20.0),
        outcome_prob_no_mod(SpectacularSuccess(QL_1), 1.0 / 20.0),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn with_extra_skill_points_modifier() {
    // Attributes 12, 12, 13 Skill value 3
    // Spectacular Success (QL 1) with remaining extra skill points: 1 / 8000 Value 1.5 (1.5 / 8000)
    // Critical Success (QL 1) with remaining extra skill points: 57 / 8000 Value 1.5 (85.5 / 8000)
    // Success (QL 1) with remaining extra skill points:
    //   ([1-12, 1-12, 1-16], [1-12, 13, 1-15], [1-12, 14, 1-14], [1-12, 15, 1-13],
    //     [13, 1-12, 1-15], [13, 13, 1-14], [13, 14, 1-13],
    //     [14, 1-12, 1-14], [14, 13, 1-13], [15, 1-12, 1-13]) -
    //   ([1, 1, 1-16], [1, 2-15, 1], [2-15, 1, 1])
    // => 3308 / 8000 Value 1.5 (4962 / 8000)
    // Success (QL 1) without remaining extra skill points:
    //   ([1-12, 1-12, 17], [1-12, 13, 16], [1-12, 14, 15], [1-12, 15, 14], [1-12, 16, 1-13],
    //     [13, 1-12, 16], [13, 13, 15], [13, 14, 14], [13, 15, 1-13],
    //     [14, 1-12, 15], [14, 13, 14], [14, 14, 1-13],
    //     [15, 1-12, 14], [15, 13, 1-13], [16, 1-12, 1-13]) -
    //   ([1, 1, 17], [1, 16, 1], [16, 1, 1])
    // => 567 / 8000 Value 1 (567 / 8000)
    // Spectacular Failure with remaining extra skill points: 1 / 8000 Value 0.5 (0.5 / 8000)
    // Critical Failure with remaining extra skill points: 57 / 8000 Value 0.5 (28.5 / 8000)
    // Failure with remaining extra skill points: 4009 / 8000 Value 0.5 (2004.5 / 8000)

    let engine = engine(
        "(if is_success then 1.0 else 0.0) + as_float(remaining_extra_skill_points(1)) * 0.5",
    );

    let extra_skill_point = Modifier::ExtraSkillPoints(SkillPoints::new(1));
    let evaluated = engine
        .evaluate_partial(PartialSkillCheckState {
            attributes: [Attribute::new(12), Attribute::new(12), Attribute::new(13)],
            skill_value: SkillPoints::new(3),
            modifiers: ModifierState::from_modifiers([extra_skill_point]),
            ..default()
        })
        .unwrap();

    let expected_probs = SkillCheckOutcomeProbabilities::from([
        outcome_prob(
            SpectacularSuccess(QL_1).with_modifiers([extra_skill_point]),
            1.0 / 8000.0,
        ),
        outcome_prob(
            CriticalSuccess(QL_1).with_modifiers([extra_skill_point]),
            57.0 / 8000.0,
        ),
        outcome_prob(
            Success(QL_1).with_modifiers([extra_skill_point]),
            3308.0 / 8000.0,
        ),
        outcome_prob_no_mod(Success(QL_1), 567.0 / 8000.0),
        outcome_prob(Failure.with_modifiers([extra_skill_point]), 4009.0 / 8000.0),
        outcome_prob(
            CriticalFailure.with_modifiers([extra_skill_point]),
            57.0 / 8000.0,
        ),
        outcome_prob(
            SpectacularFailure.with_modifiers([extra_skill_point]),
            1.0 / 8000.0,
        ),
    ]);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
    assert_that!(evaluated.evaluation).is_close_to(eval(7649.5 / 8000.0), EPS);
}

#[test]
fn evaluation_error() {
    // Fails at QL 4
    let engine = engine("100 / (4 - quality_level)");
    let state = PartialSkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        skill_value: SkillPoints::new(18),
        ..default()
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
        ..default()
    };

    let result = engine.evaluate_partial(state);

    assert_that!(result).contains_error(RuntimeError {
        kind: RuntimeErrorKind::InvalidResult(f64::INFINITY),
        span: (0..33).into(),
    });
}
