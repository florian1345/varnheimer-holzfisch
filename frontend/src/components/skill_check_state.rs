use dioxus::prelude::*;
use model::check::modifier::{Aptitude, Modifier};
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::roll::Roll;
use model::skill;

use crate::clone_env;
use crate::components::basic_fields::{
    AptitudeInput,
    AttributeInput,
    FatePointInput,
    RollInput,
    SkillPointsInput,
};
use crate::components::flex_break::FlexBreak;

fn exchange_attribute(
    state: &PartialSkillCheckState,
    index: usize,
    new_attribute: skill::Attribute,
) -> PartialSkillCheckState {
    let mut new_attributes = state.attributes;
    new_attributes[index] = new_attribute;

    PartialSkillCheckState {
        attributes: new_attributes,
        ..state.clone()
    }
}

fn exchange_roll(
    state: &PartialSkillCheckState,
    index: usize,
    new_roll: Option<Roll>,
) -> PartialSkillCheckState {
    let mut new_fixed_rolls = state.fixed_rolls;
    new_fixed_rolls[index] = new_roll;

    PartialSkillCheckState {
        fixed_rolls: new_fixed_rolls,
        ..state.clone()
    }
}

fn get_aptitude(state: &PartialSkillCheckState) -> Option<Aptitude> {
    state.modifiers.available_modifiers().find_map(|modifier| {
        if let Modifier::Aptitude(aptitude) = modifier {
            Some(aptitude)
        }
        else {
            None
        }
    })
}

fn set_aptitude(state: &PartialSkillCheckState, value: Option<Aptitude>) -> PartialSkillCheckState {
    let mut new_state = state.clone();

    while let Some(aptitude) = get_aptitude(&new_state) {
        new_state.modifiers.consume(Modifier::Aptitude(aptitude));
    }

    if let Some(aptitude) = value {
        new_state.modifiers.add(Modifier::Aptitude(aptitude));
    }

    new_state
}

#[derive(PartialEq, Clone, Props)]
pub struct SkillCheckStateFormProps {
    skill_check_state: PartialSkillCheckState,
    onchange: EventHandler<PartialSkillCheckState>,
}

#[component]
pub fn SkillCheckStateForm(props: SkillCheckStateFormProps) -> Element {
    rsx! {
        div {
            class: "skill-check-state-form center-box",

            "Attributes",

            AttributeInput {
                attribute: props.skill_check_state.attributes[0],
                onchange: clone_env! { props -> move |new_attribute| props.onchange.call(
                    exchange_attribute(&props.skill_check_state, 0, new_attribute)
                )},
            },
            AttributeInput {
                attribute: props.skill_check_state.attributes[1],
                onchange: clone_env! { props -> move |new_attribute| props.onchange.call(
                    exchange_attribute(&props.skill_check_state, 1, new_attribute)
                )},
            },
            AttributeInput {
                attribute: props.skill_check_state.attributes[2],
                onchange: clone_env! { props -> move |new_attribute| props.onchange.call(
                    exchange_attribute(&props.skill_check_state, 2, new_attribute)
                )},
            },

            "Skill Value",

            SkillPointsInput {
                skill_points: props.skill_check_state.skill_value,
                onchange: clone_env! { props -> move |new_skill_value| props.onchange.call(
                    PartialSkillCheckState {
                        skill_value: new_skill_value,
                        ..props.skill_check_state.clone()
                    }
                )}
            },

            FlexBreak {},

            "Roll",

            RollInput {
                roll: props.skill_check_state.fixed_rolls[0],
                onchange: clone_env! { props -> move |new_roll| props.onchange.call(
                    exchange_roll(&props.skill_check_state, 0, new_roll)
                )},
            },
            RollInput {
                roll: props.skill_check_state.fixed_rolls[1],
                onchange: clone_env! { props -> move |new_roll| props.onchange.call(
                    exchange_roll(&props.skill_check_state, 1, new_roll)
                )},
            },
            RollInput {
                roll: props.skill_check_state.fixed_rolls[2],
                onchange: clone_env! { props -> move |new_roll| props.onchange.call(
                    exchange_roll(&props.skill_check_state, 2, new_roll)
                )},
            },

            FlexBreak {},

            "Fate Points",

            FatePointInput {
                fate_point_count: props.skill_check_state.modifiers.count_of(Modifier::FatePoint),
                onchange: clone_env! { props -> move |new_fate_point_count| {
                    // TODO adapt the UI to represent the domain model more (list of modifiers)
                    let mut new_state = props.skill_check_state.clone();

                    while new_state.modifiers.count_of(Modifier::FatePoint) < new_fate_point_count {
                        new_state.modifiers.add(Modifier::FatePoint);
                    }

                    while new_state.modifiers.count_of(Modifier::FatePoint) > new_fate_point_count {
                        new_state.modifiers.consume(Modifier::FatePoint);
                    }

                    props.onchange.call(new_state)
                }}
            },

            "Aptitude Dice",

            AptitudeInput {
                aptitude: get_aptitude(&props.skill_check_state),
                onchange: clone_env! { props -> move |new_aptitude|
                    props.onchange.call(set_aptitude(&props.skill_check_state, new_aptitude))
                },
            }
        }
    }
}
