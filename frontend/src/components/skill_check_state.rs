use dioxus::prelude::*;
use model::check::modifier::{Aptitude, Modifier};
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::roll::Roll;
use model::skill;

use crate::components::flex_break::FlexBreak;
use crate::components::model_value_fields::{
    AptitudeInput,
    AttributeInput,
    FatePointInput,
    RollInput,
    SkillPointsInput,
};

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

fn exchange<T, const LEN: usize>(mut array: [T; LEN], index: usize, value: T) -> [T; LEN] {
    array[index] = value;
    array
}

#[component]
fn AttributesForm(
    attributes: [skill::Attribute; DICE_PER_SKILL_CHECK],
    on_change: EventHandler<[skill::Attribute; DICE_PER_SKILL_CHECK]>,
) -> Element {
    rsx! {
        for index in 0..DICE_PER_SKILL_CHECK {
            AttributeInput {
                attribute: attributes[index],
                on_change: move |new_attribute| {
                    on_change(exchange(attributes, index, new_attribute));
                },
                key: "attribute-{index}",
            }
        }
    }
}

#[component]
fn RollsForm(
    rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
    on_change: EventHandler<[Option<Roll>; DICE_PER_SKILL_CHECK]>,
) -> Element {
    rsx! {
        for index in 0..DICE_PER_SKILL_CHECK {
            RollInput {
                roll: rolls[index],
                on_change: move |new_roll| on_change(exchange(rolls, index, new_roll)),
                key: "roll-{index}",
            }
        }
    }
}

#[component]
pub fn SkillCheckStateForm(skill_check_state: Signal<PartialSkillCheckState>) -> Element {
    rsx! {
        div {
            class: "skill-check-state-form center-box",

            "Attributes",

            AttributesForm {
                attributes: skill_check_state().attributes,
                on_change: move |attributes| skill_check_state.write().attributes = attributes,
            },

            "Skill Value",

            SkillPointsInput {
                skill_points: skill_check_state().skill_value,
                on_change: move |skill_value| skill_check_state.write().skill_value = skill_value,
            },

            FlexBreak {},

            "Roll",

            RollsForm {
                rolls: skill_check_state().fixed_rolls,
                on_change: move |rolls| skill_check_state.write().fixed_rolls = rolls,
            },

            FlexBreak {},

            // TODO adapt the UI to represent the domain model more (list of modifiers)

            "Fate Points",

            FatePointInput {
                fate_point_count: skill_check_state().modifiers.count_of(Modifier::FatePoint),
                on_change: move |fate_point_count| {
                    let mut skill_check_state = skill_check_state.write();
                    skill_check_state.modifiers
                        .retain(|modifier| modifier != &Modifier::FatePoint);

                    for _ in 0..fate_point_count {
                        skill_check_state.modifiers.add(Modifier::FatePoint);
                    }
                }
            },

            "Aptitude Dice",

            AptitudeInput {
                aptitude: get_aptitude(&skill_check_state()),
                on_change: move |aptitude| {
                    let mut skill_check_state = skill_check_state.write();
                    skill_check_state.modifiers
                        .retain(|modifier| !matches!(modifier, Modifier::Aptitude(_)));

                    if let Some(aptitude) = aptitude {
                        skill_check_state.modifiers.add(Modifier::Aptitude(aptitude));
                    }
                }
            }
        }
    }
}
