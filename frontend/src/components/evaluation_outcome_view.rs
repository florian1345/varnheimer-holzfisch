use std::fmt::Write;

use dioxus::prelude::*;
use model::check::SkillCheckAction;
use model::check::modifier::{Modifier, ModifierAction};

use crate::components::evaluated_probabilities_view::EvaluatedProbabilitiesView;
use crate::display::EnumerationDisplay;
use crate::evaluate::EvaluationOutcome;

fn format_modifier(modifier: &Modifier, output: &mut String) -> bool {
    match modifier {
        Modifier::FatePoint => {
            write!(output, "fate point").unwrap();
            true
        },
        Modifier::Aptitude(_) => {
            write!(output, "aptitude").unwrap();
            true
        },
        Modifier::ExtraSkillPoints(skill_points) => {
            write!(output, "{} extra skill points", skill_points.as_i32()).unwrap();
            false
        },
        Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
            write!(
                output,
                "{} extra skill points on success",
                skill_points.as_i32()
            )
            .unwrap();
            false
        },
        Modifier::ExtraQualityLevelOnSuccess => {
            write!(output, "extra quality level on success").unwrap();
            false
        },
    }
}

fn format_modifier_action(action: &ModifierAction, output: &mut String) {
    match action {
        ModifierAction::RerollByFate(reroll) | ModifierAction::RerollByAptitude(reroll) => {
            let die_numbers = reroll
                .pattern()
                .into_iter()
                .enumerate()
                .filter(|&(_, reroll)| reroll)
                .map(|(index, _)| index + 1)
                .collect::<Vec<_>>();
            let die_numbers_display = EnumerationDisplay::new(&die_numbers, "and");

            write!(
                output,
                "reroll {} {}",
                if die_numbers.len() > 1 { "dice" } else { "die" },
                die_numbers_display
            )
            .unwrap();
        },
        ModifierAction::IncreaseSkillPoints(skill_points) => {
            write!(output, "increase skill points by {}", skill_points.as_i32()).unwrap()
        },
        ModifierAction::IncreaseSkillPointsOnSuccess(skill_points) => write!(
            output,
            "increase skill points on success by {}",
            skill_points.as_i32()
        )
        .unwrap(),
        ModifierAction::IncreaseQualityLevel => write!(output, "increase quality level").unwrap(),
    }
}

fn format_action(action: &SkillCheckAction) -> String {
    match action {
        SkillCheckAction::Accept => "Recommendation: Accept".to_owned(),
        SkillCheckAction::ConsumeModifier { modifier, action } => {
            let mut output = "Recommendation: Use ".to_owned();

            if format_modifier(modifier, &mut output) {
                output.push_str(" to ");
                format_modifier_action(action, &mut output);
            }

            output
        },
    }
}

#[component]
pub fn EvaluationOutcomeView(evaluation_outcome: EvaluationOutcome) -> Element {
    rsx! {
        div {
            class: "center-box",

            if let Some(recommended_action) = evaluation_outcome.recommended_action {
                div {
                    class: "info",

                    { format_action(&recommended_action) }
                }
            }

            EvaluatedProbabilitiesView {
                evaluated_probabilities: evaluation_outcome.evaluated_probabilities,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use model::check::modifier::{Aptitude, Modifier, ModifierAction, Reroll};
    use model::check::outcome::{
        SkillCheckOutcome,
        SkillCheckOutcomeKind,
        SkillCheckOutcomeProbabilities,
    };
    use model::evaluation::{Evaluated, Evaluation};
    use model::skill::SkillPoints;
    use rstest::rstest;

    use super::*;
    use crate::evaluate::EvaluationOutcome;

    fn mount(evaluation_outcome: EvaluationOutcome) -> TestDom {
        TestDom::new_with_props(
            EvaluationOutcomeView,
            EvaluationOutcomeViewProps { evaluation_outcome },
        )
    }

    #[test]
    fn displays_evaluation_without_recommended_action() {
        let dom = mount(EvaluationOutcome {
            recommended_action: None,
            evaluated_probabilities: Evaluated {
                evaluated: always_failure(),
                evaluation: Evaluation::new(0.25).unwrap(),
            },
        });

        let info_div = dom.find("div.center-box > div.info");

        // We are content with only checking the existence of an EvaluatedProbabilitiesView.
        // Table content will be tested in its own test.
        assert_that!(&info_div.children()[0]).is_text("Average value: 0.25");
        assert_that!(dom.try_find("div.error")).is_none();
    }

    fn always_failure() -> SkillCheckOutcomeProbabilities {
        SkillCheckOutcomeProbabilities::of_known_outcome(SkillCheckOutcome {
            kind: SkillCheckOutcomeKind::Failure,
            remaining_modifiers: Default::default(),
        })
    }

    #[rstest]
    #[case::accept(SkillCheckAction::Accept, "Recommendation: Accept")]
    #[case::fate_point_reroll_single_die(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::FatePoint,
            action: ModifierAction::RerollByFate(Reroll::new([false, true, false]).unwrap()),
            },
        "Recommendation: Use fate point to reroll die 2",
    )]
    #[case::fate_point_reroll_two_dice(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::FatePoint,
            action: ModifierAction::RerollByFate(Reroll::new([true, false, true]).unwrap()),
            },
        "Recommendation: Use fate point to reroll dice 1 and 3",
    )]
    #[case::fate_point_reroll_all_dice(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::FatePoint,
            action: ModifierAction::RerollByFate(Reroll::new([true, true, true]).unwrap()),
            },
        "Recommendation: Use fate point to reroll dice 1, 2, and 3",
    )]
    #[case::fate_point_increase_quality_level(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::FatePoint,
            action: ModifierAction::IncreaseQualityLevel,
            },
        "Recommendation: Use fate point to increase quality level",
    )]
    #[case::aptitude_reroll_single_die(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::Aptitude(Aptitude::new(1).unwrap()),
            action: ModifierAction::RerollByAptitude(Reroll::new([true, false, false]).unwrap()),
            },
        "Recommendation: Use aptitude to reroll die 1",
    )]
    #[case::aptitude_reroll_two_dice(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::Aptitude(Aptitude::new(2).unwrap()),
            action: ModifierAction::RerollByAptitude(Reroll::new([false, true, true]).unwrap()),
            },
        "Recommendation: Use aptitude to reroll dice 2 and 3",
    )]
    #[case::extra_skill_points(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::ExtraSkillPoints(SkillPoints::new(2)),
            action: ModifierAction::IncreaseSkillPoints(SkillPoints::new(2)),
            },
        "Recommendation: Use 2 extra skill points",
    )]
    #[case::extra_skill_points_on_success(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(2)),
            action: ModifierAction::IncreaseSkillPointsOnSuccess(SkillPoints::new(2)),
            },
        "Recommendation: Use 2 extra skill points on success",
    )]
    #[case::extra_quality_level_on_success(
        SkillCheckAction::ConsumeModifier{
            modifier: Modifier::ExtraQualityLevelOnSuccess,
            action: ModifierAction::IncreaseQualityLevel,
            },
        "Recommendation: Use extra quality level on success",
    )]
    fn displays_recommended_action(
        #[case] recommended_action: SkillCheckAction,
        #[case] expected_text: &str,
    ) {
        let dom = mount(EvaluationOutcome {
            recommended_action: Some(recommended_action),
            evaluated_probabilities: Evaluated {
                evaluated: always_failure(),
                evaluation: Evaluation::new(0.25).unwrap(),
            },
        });

        assert_that!(dom.find("div.center-box").children()).has_length(2);

        let recommended_action_div = dom.find_first(".info").unwrap();

        assert_that!(recommended_action_div).contains_only_text(expected_text);
    }
}
