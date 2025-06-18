use std::fmt::Write;

use dioxus::prelude::*;
use model::check::SkillCheckAction;
use model::check::modifier::{Modifier, ModifierAction};

use crate::components::evaluated_probabilities_view::EvaluatedProbabilitiesView;
use crate::evaluate::EvaluationResult;

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
            let dice_count = die_numbers.len();

            if dice_count == 1 {
                write!(output, "reroll die {}", die_numbers[0]).unwrap();
            }
            else if dice_count == 2 {
                write!(
                    output,
                    "reroll dice {} and {}",
                    die_numbers[0], die_numbers[1]
                )
                .unwrap();
            }
            else {
                write!(
                    output,
                    "reroll dice {}, and {}",
                    die_numbers[0..(dice_count - 1)]
                        .iter()
                        .map(|number| format!("{}", number))
                        .collect::<Vec<_>>()
                        .join(", "),
                    die_numbers[dice_count - 1]
                )
                .unwrap();
            }
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
pub fn EvaluationResultView(evaluation_result: EvaluationResult) -> Element {
    rsx! {
        div {
            class: "center-box",

            match evaluation_result {
                Ok(evaluation_outcome) => rsx! {
                    if let Some(recommended_action) = evaluation_outcome.recommended_action {
                        div {
                            class: "info",

                            { format_action(&recommended_action) }
                        }
                    }

                    EvaluatedProbabilitiesView {
                        evaluated_probabilities: evaluation_outcome.evaluated_probabilities,
                    }
                },
                Err(error) => rsx! {
                    div {
                        class: "error",

                        { format!("{}", error) }
                    }
                },
            }
        }
    }
}
