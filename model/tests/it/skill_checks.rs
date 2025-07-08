use kernal::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierAction, ModifierState, Reroll};
use model::check::outcome::SkillCheckOutcomeProbabilities;
use model::check::{SkillCheckAction, SkillCheckState};
use model::evaluation::Evaluation;
use model::roll::Roll;
use model::skill::{Attribute, SkillPoints};
use rstest::rstest;

use crate::*;

fn roll(roll: u8) -> Roll {
    Roll::new(roll).unwrap()
}

fn default() -> SkillCheckState {
    SkillCheckState {
        attributes: [Attribute::new(10), Attribute::new(10), Attribute::new(10)],
        rolls: [roll(10), roll(10), roll(10)],
        skill_value: SkillPoints::new(0),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: ModifierState::default(),
    }
}

#[test]
fn given_roll_with_fate_point_evaluates_options_correctly() {
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(11), Attribute::new(10)],
        rolls: [roll(16), roll(4), roll(10)],
        skill_value: SkillPoints::new(7),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        ..default()
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
        rolls: [roll(13), roll(10), roll(12)],
        skill_value: SkillPoints::new(8),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        ..default()
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
        rolls: [roll(16), roll(4), roll(14)],
        skill_value: SkillPoints::new(5),
        modifiers: ModifierState::from_modifiers([aptitude_1]),
        ..default()
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
        rolls: [roll(3), roll(8), roll(5)],
        skill_value: SkillPoints::new(7),
        modifiers: ModifierState::from_modifiers([aptitude_1]),
        ..default()
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
fn given_aptitude_and_fate_point_prefers_aptitude_when_appropriate() {
    // To get max QL, it is best to reroll the 16 by aptitude and save the fate point for a
    // potential QL increase.
    // If reroll is 0-11, increase QL after for QL 3 (55 %)
    // If reroll is 12-15, increase QL after for QL 2 (20 %)
    // If reroll is 16-20, reroll again with fate point (25 %)
    //   If reroll is 0-11, we get QL 2 (25 % * 55 % = 13.75 %)
    //   If reroll is 12-15, we get QL 1 (25 % * 20 % = 5 %)
    //   If reroll is 16-20, we get Failure (25 % * 25 % = 6.25 %)

    let aptitude_1 = Modifier::Aptitude(Aptitude::new(1).unwrap());
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(11), Attribute::new(12), Attribute::new(13)],
        rolls: [roll(16), roll(8), roll(5)],
        skill_value: SkillPoints::new(4),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint, aptitude_1]),
        ..default()
    };

    let engine = engine("quality_level");

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();
    let best_move = evaluated[0].clone();

    assert_that!(best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: aptitude_1,
        action: ModifierAction::RerollByAptitude(Reroll::new([true, false, false]).unwrap()),
    });
    assert_that!(best_move.1.evaluated).is_close_to(
        SkillCheckOutcomeProbabilities::from([
            outcome_prob(SkillCheckOutcomeKind::Success(QL_3), 0.55),
            outcome_prob(SkillCheckOutcomeKind::Success(QL_2), 0.3375),
            outcome_prob(SkillCheckOutcomeKind::Success(QL_1), 0.05),
            outcome_prob(SkillCheckOutcomeKind::Failure, 0.0625),
        ]),
        EPS,
    );
    assert_that!(best_move.1.evaluation).is_close_to(eval(2.375), EPS);
}

#[test]
fn given_aptitude_and_fate_point_prefers_fate_point_when_appropriate() {
    // Optimization goal is success probability.
    // Every die needs to be rerolled anyway, so it is optimal to hold back the aptitude to react to
    // any dice still over the attribute after rerolling.
    // We succeed if
    // - the fate point rolls a critical success
    //   - Probability: (19 * 3 + 1) / 8000 = 58 / 8000
    // - the fate point rolls each die <= 12 immediately, but not a critical success
    //   - Probability: (12 * 12 * 12 - (11 * 3 + 1)) / 8000 = 1694 / 8000
    // - the fate point rolls two dice <= 12 and one die > 12, but not a critical success
    //   - Probability: (12 * 12 * 8 * 3 - 8 * 3) / 8000 = 3432 / 8000
    //   - AND the aptitude-reroll is <= 12 (Probability 60 %)
    //   - Combined Probability: 41184 / 160000
    // - the fate point rolls exactly one 1 and the others > 12, but not a critical failure
    //   - Probability: (8 * 8 * 3 - 3) / 8000 = 189 / 8000
    //   - AND the aptitude-reroll rolls a 1 (Probability 5 %)
    //   - Combined Probability: 189 / 160000

    let aptitude_1 = Modifier::Aptitude(Aptitude::new(1).unwrap());
    let skill_check_state = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(12), Attribute::new(12)],
        rolls: [roll(18), roll(18), roll(18)],
        skill_value: SkillPoints::new(0),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint, aptitude_1]),
        ..default()
    };

    let engine = engine("if is_success then 1 else 0");

    let evaluated = engine.evaluate_all_actions(skill_check_state).unwrap();
    let best_move = evaluated[0].clone();

    assert_that!(best_move.0).is_equal_to(SkillCheckAction::ConsumeModifier {
        modifier: Modifier::FatePoint,
        action: ModifierAction::RerollByFate(Reroll::new([true, true, true]).unwrap()),
    });
    // No assertion of exact probabilities: since the value of aptitude is not specified, it may or
    // may not be used after the fate point succeeds anyway.
    assert_that!(best_move.1.evaluation).is_close_to(eval(76_413.0 / 160_000.0), EPS);
}
