use kernal::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierAction, ModifierState, Reroll};
use model::check::outcome::{
    SkillCheckOutcome,
    SkillCheckOutcomeKind,
    SkillCheckOutcomeProbabilities,
};
use model::check::{PartialSkillCheckState, SkillCheckAction, SkillCheckState};
use model::engine::VarnheimerHolzfischEngine;
use model::evaluation::Evaluation;
use model::probability::Probability;
use model::roll::Roll;
use model::skill::{Attribute, QualityLevel, SkillPoints};
use rstest::rstest;
use scholle::ScholleEvaluator;
use scholle::error::{RuntimeError, RuntimeErrorKind};

fn engine(scholle_code: impl AsRef<str>) -> VarnheimerHolzfischEngine<ScholleEvaluator> {
    let evaluator = ScholleEvaluator::new(scholle_code).unwrap();
    VarnheimerHolzfischEngine { evaluator }
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
        remaining_modifiers: ModifierState::default(),
    }
}

const EPS: f64 = 1e-6;

#[test]
fn simple_call_with_zero_skill_points() {
    let success_eval = eval(1.0);
    let critical_success_eval = eval(1_000.0);
    let spectacular_success_eval = eval(1_000_000.0);
    let success = outcome_no_fate_points(SkillCheckOutcomeKind::Success(QualityLevel::ONE));
    let critical_success =
        outcome_no_fate_points(SkillCheckOutcomeKind::CriticalSuccess(QualityLevel::ONE));
    let spectacular_success =
        outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularSuccess(QualityLevel::ONE));
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
            roll_caps: [None, None, None],
            fixed_rolls: [None, None, None],
            skill_value: SkillPoints::new(0),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
            modifiers: ModifierState::default(),
            inaptitude: false,
        })
        .unwrap();

    let success_probability = prob((1000.0 - 28.0) / 8000.0);
    let critical_success_probability = prob(57.0 / 8000.0);
    let spectacular_success_probability = prob(1.0 / 8000.0);
    let expected_probs = SkillCheckOutcomeProbabilities::from([
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularFailure),
            prob(1.0 / 8000.0),
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::CriticalFailure),
            prob(57.0 / 8000.0),
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::Failure),
            prob((7000.0 - 88.0) / 8000.0),
        ),
        (success, success_probability),
        (critical_success, critical_success_probability),
        (spectacular_success, spectacular_success_probability),
    ]);
    let expected_evaluation = success_eval * success_probability
        + critical_success_eval * critical_success_probability
        + spectacular_success_eval * spectacular_success_probability;

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
            roll_caps: [None, None, None],
            fixed_rolls: [None, None, None],
            skill_value: SkillPoints::new(0),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
            modifiers: ModifierState::default(),
            inaptitude: true,
        })
        .unwrap();

    let success_probability = prob(9_963.0 / 160_000.0);
    let critical_success_probability = prob(76.0 / 160_000.0);
    let spectacular_success_probability = prob(1.0 / 160_000.0);
    let expected_probs = SkillCheckOutcomeProbabilities::from([
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularFailure),
            prob(58.0 / 160_000.0),
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::CriticalFailure),
            prob(2_185.0 / 160_000.0),
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::Failure),
            prob(147_717.0 / 160_000.0),
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::Success(QualityLevel::ONE)),
            success_probability,
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::CriticalSuccess(QualityLevel::ONE)),
            critical_success_probability,
        ),
        (
            outcome_no_fate_points(SkillCheckOutcomeKind::SpectacularSuccess(QualityLevel::ONE)),
            spectacular_success_probability,
        ),
    ]);
    let expected_evaluation = Evaluation::new(1.0).unwrap()
        * (success_probability + critical_success_probability + spectacular_success_probability);

    assert_that!(evaluated.evaluation).is_close_to(expected_evaluation, EPS);
    assert_that!(evaluated.evaluated).is_close_to(expected_probs, EPS);
}

#[test]
fn given_roll_with_fate_point_evaluates_options_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(11), Attribute::new(10)],
        rolls: [
            Roll::new(16).unwrap(),
            Roll::new(4).unwrap(),
            Roll::new(10).unwrap(),
        ],
        skill_value: SkillPoints::new(7),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
    };

    let engine = engine("quality_level");

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();

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
    assert_that!(best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: Modifier::FatePoint,
        action: ModifierAction::RerollByFate(Reroll::new([true, false, false]).unwrap()),
    });
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.3), EPS);
    assert_that!(second_best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: Modifier::FatePoint,
        action: ModifierAction::IncreaseQualityLevel,
    });
    assert_that!(second_best_move.1.evaluation).is_close_to(eval(2.0), EPS);
}

#[test]
fn given_roll_with_fate_point_evaluates_cost_of_fate_point_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(9), Attribute::new(13), Attribute::new(11)],
        rolls: [
            Roll::new(13).unwrap(),
            Roll::new(10).unwrap(),
            Roll::new(12).unwrap(),
        ],
        skill_value: SkillPoints::new(8),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
    };

    let engine = engine("as_float(quality_level) + 1.5 * as_float(remaining_fate_points)");

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();

    // None of the options yields an average increase of >1.5 QL, hence accepting is best.
    // The value is the achieved QL (1) + the value of remaining fate points (1.5).

    let best_move = evaluated[0].clone();

    assert_that!(evaluated).has_length(9); // accept + increase QL + 7 reroll combos
    assert_that!(best_move.0).is_equal_to(SkillCheckAction::Accept);
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.5), EPS);
}

#[rstest]
#[case::max_success_probability(
    "if is_success then 1 else 0",
    ModifierAction::RerollByAptitude(Reroll::new([true, false, false]).unwrap()),
    eval(0.75)
)]
#[case::max_average_ql(
    "quality_level",
    ModifierAction::RerollByAptitude(Reroll::new([false, false, true]).unwrap()),
    eval(1.1)
)]
fn given_roll_with_aptitude_evaluates_options_correctly(
    #[case] scholle_code: &str,
    #[case] expected_best_action: ModifierAction,
    #[case] expected_evaluation: Evaluation,
) {
    // There are only 2 reasonable options: reroll 16 or 14. Which is better depends on evaluator:
    // - 16 gives 75% chance of success, but only at QL 1
    // - 14 gives 65% chance of success, but 45% chance of QL 2

    let aptitude_1 = Modifier::Aptitude(Aptitude::new(1).unwrap());
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(15), Attribute::new(14), Attribute::new(9)],
        rolls: [
            Roll::new(16).unwrap(),
            Roll::new(4).unwrap(),
            Roll::new(14).unwrap(),
        ],
        skill_value: SkillPoints::new(5),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::from_modifiers([aptitude_1]),
    };

    let engine = engine(scholle_code);

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();
    let best_move = evaluated[0].clone();

    assert_that!(evaluated).has_length(4); // accept + reroll each die
    assert_that!(best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: aptitude_1,
        action: expected_best_action,
    });
    assert_that!(best_move.1.evaluation).is_close_to(expected_evaluation, EPS);
}

#[test]
fn given_roll_with_aptitude_evaluates_reroll_with_cap_correctly() {
    // Although rerolling the 8 would come with a risk of decreasing the quality or failing the
    // check, since aptitude cannot make the result worse, it is worth rerolling.
    // Result: 35 % QL 3, 65 % QL 2 => value 2.35

    let aptitude_1 = Modifier::Aptitude(Aptitude::new(1).unwrap());
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(9), Attribute::new(7), Attribute::new(9)],
        rolls: [
            Roll::new(3).unwrap(),
            Roll::new(8).unwrap(),
            Roll::new(5).unwrap(),
        ],
        skill_value: SkillPoints::new(7),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::from_modifiers([aptitude_1]),
    };

    let engine = engine("quality_level");

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();
    let best_move = evaluated[0].clone();

    assert_that!(evaluated).has_length(4); // accept + reroll each die
    assert_that!(best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: aptitude_1,
        action: ModifierAction::RerollByAptitude(Reroll::new([false, true, false]).unwrap()),
    });
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.35), EPS);
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
