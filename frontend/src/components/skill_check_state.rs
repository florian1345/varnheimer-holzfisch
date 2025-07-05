use dioxus::prelude::*;
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::roll::Roll;
use model::skill;

use crate::components::flex_break::FlexBreak;
use crate::components::model_value_fields::{AttributeInput, RollInput, SkillPointsInput};
use crate::components::modifier_state::ModifierStateInput;

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

            h3 {
                "Modifiers"
            }

            FlexBreak {},

            ModifierStateInput {
                modifier_state: skill_check_state().modifiers.clone(),
                on_change: move |modifiers| skill_check_state.write().modifiers = modifiers,
            }
        }
    }
}
