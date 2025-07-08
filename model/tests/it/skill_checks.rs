use kernal::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierAction, ModifierState, Reroll};
use model::check::{SkillCheckAction, SkillCheckState};
use model::evaluation::Evaluation;
use model::roll::Roll;
use model::skill::{Attribute, SkillPoints};
use rstest::rstest;

use crate::*;

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
