use dioxus::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierState};
use model::skill::SkillPoints;

use crate::components::flex_spacer::FlexSpacer;
use crate::components::model_value_fields::{AptitudeInput, SkillPointsInput};
use crate::components::number_input::NumberInput;

const MODIFIER_ID_FATE_POINT: &str = "fate-point";
const MODIFIER_ID_APTITUDE: &str = "aptitude";
const MODIFIER_ID_EXTRA_SKILL_POINTS: &str = "extra-skill-points";
const MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS: &str = "extra-skill-points-on-success";
const MODIFIER_ID_EXTRA_QUALITY_LEVEL: &str = "extra-quality-level";

fn modifier_from_id(id: &str) -> Option<Modifier> {
    match id {
        MODIFIER_ID_FATE_POINT => Some(Modifier::FatePoint),
        MODIFIER_ID_APTITUDE => Some(Modifier::Aptitude(Aptitude::new(1).unwrap())),
        MODIFIER_ID_EXTRA_SKILL_POINTS => Some(Modifier::ExtraSkillPoints(SkillPoints::new(1))),
        MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS => {
            Some(Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(1)))
        },
        MODIFIER_ID_EXTRA_QUALITY_LEVEL => Some(Modifier::ExtraQualityLevelOnSuccess),
        _ => None,
    }
}

fn modifier_id(modifier: Modifier) -> &'static str {
    match modifier {
        Modifier::FatePoint => MODIFIER_ID_FATE_POINT,
        Modifier::Aptitude(_) => MODIFIER_ID_APTITUDE,
        Modifier::ExtraSkillPoints(_) => MODIFIER_ID_EXTRA_SKILL_POINTS,
        Modifier::ExtraSkillPointsOnSuccess(_) => MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS,
        Modifier::ExtraQualityLevelOnSuccess => MODIFIER_ID_EXTRA_QUALITY_LEVEL,
    }
}

#[component]
fn NewModifierInput(on_added: EventHandler<Modifier>) -> Element {
    let mut modifier = use_signal(|| None);

    rsx! {
        div {
            class: "modifier-row",

            select {
                value: modifier().map(modifier_id).unwrap_or(""),
                onchange: move |event| {
                    modifier.set(Some(modifier_from_id(&event.value()).unwrap()));
                },

                option {
                    hidden: true,
                    value: "",
                    ""
                }

                option {
                    value: MODIFIER_ID_FATE_POINT,
                    "Fate point"
                }

                option {
                    value: MODIFIER_ID_APTITUDE,
                    "Aptitude"
                }

                option {
                    value: MODIFIER_ID_EXTRA_SKILL_POINTS,
                    "Extra skill points"
                }

                option {
                    value: MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS,
                    "Extra skill points on success"
                }

                option {
                    value: MODIFIER_ID_EXTRA_QUALITY_LEVEL,
                    "Extra quality level"
                }
            }

            if let Some(Modifier::Aptitude(aptitude)) = modifier() {
                "Dice"

                AptitudeInput {
                    aptitude: aptitude,
                    on_change: move |aptitude| {
                        modifier.set(Some(Modifier::Aptitude(aptitude)));
                    }
                }
            }

            if let Some(Modifier::ExtraSkillPoints(skill_points)) = modifier() {
                "Skill points"

                SkillPointsInput {
                    skill_points: skill_points,
                    on_change: move |skill_points| {
                        modifier.set(Some(Modifier::ExtraSkillPoints(skill_points)));
                    },
                    non_zero: true,
                }
            }

            if let Some(Modifier::ExtraSkillPointsOnSuccess(skill_points)) = modifier() {
                "Skill points"

                SkillPointsInput {
                    skill_points: skill_points,
                    on_change: move |skill_points| {
                        modifier.set(Some(Modifier::ExtraSkillPointsOnSuccess(skill_points)));
                    },
                    non_zero: true,
                }
            }

            FlexSpacer {}

            button {
                class: "end-button",
                disabled: modifier().is_none(),
                onclick: move |_| {
                    if let Some(modifier) = modifier() {
                        on_added(modifier);
                    }

                    modifier.set(None);
                },

                i {
                    class: "material-icons",

                    "add"
                }
            }
        }
    }
}

fn format_modifier(modifier: Modifier) -> String {
    match modifier {
        Modifier::FatePoint => "Fate point".to_owned(),
        Modifier::Aptitude(aptitude) => {
            format!(
                "Aptitude with {} {}",
                aptitude.max_dice().get(),
                if aptitude.max_dice().get() == 1 {
                    "die"
                }
                else {
                    "dice"
                }
            )
        },
        Modifier::ExtraSkillPoints(skill_points) => {
            format!(
                "{} extra skill {}",
                skill_points.as_i32(),
                if skill_points.as_i32() == 1 {
                    "point"
                }
                else {
                    "points"
                }
            )
        },
        Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
            format!(
                "{} extra skill {} on success",
                skill_points.as_i32(),
                if skill_points.as_i32() == 1 {
                    "point"
                }
                else {
                    "points"
                }
            )
        },
        Modifier::ExtraQualityLevelOnSuccess => "Extra quality level on success".to_owned(),
    }
}

fn modifier_key(modifier: Modifier) -> String {
    match modifier {
        Modifier::FatePoint => "fate-point".to_owned(),
        Modifier::Aptitude(aptitude) => {
            format!("aptitude-{}", aptitude.max_dice().get())
        },
        Modifier::ExtraSkillPoints(skill_points) => {
            format!("skill-point-{}", skill_points.as_i32())
        },
        Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
            format!("skill-point-success-{}", skill_points.as_i32())
        },
        Modifier::ExtraQualityLevelOnSuccess => "quality-level".to_owned(),
    }
}

#[component]
fn ModifierRow(modifier: Modifier, count: usize, on_count_change: EventHandler<usize>) -> Element {
    rsx! {
        div {
            div {
                class: "modifier-row",

                { format_modifier(modifier) }

                FlexSpacer {}

                "Uses"

                NumberInput::<usize> {
                    class: "small-input",
                    min: 1,
                    value: count,
                    on_change: on_count_change,
                }


                button {
                    class: "end-button",
                    onclick: move |_| on_count_change(0),

                    i {
                        class: "material-icons",

                        "delete"
                    }
                }
            }

            hr {}
        }
    }
}

#[component]
pub fn ModifierStateInput(
    modifier_state: ModifierState,
    on_change: EventHandler<ModifierState>,
) -> Element {
    rsx! {
        div {
            class: "modifier-state-input",

            for modifier in modifier_state.available_modifiers() {
                {
                    let modifier_state = modifier_state.clone();

                    rsx! {
                        ModifierRow {
                            modifier: modifier,
                            count: modifier_state.count_of(modifier),
                            on_count_change: move |target_count| {
                                let mut new_modifier_state = modifier_state.clone();
                                new_modifier_state.set_count(modifier, target_count);
                                on_change(new_modifier_state);
                            },
                            key: "{modifier_key(modifier)}"
                        }
                    }
                }
            }

            NewModifierInput {
                on_added: move |modifier| {
                    let mut new_modifier_state = modifier_state.clone();
                    new_modifier_state.add(modifier);
                    on_change(new_modifier_state);
                }
            }
        }
    }
}
