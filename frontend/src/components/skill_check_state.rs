use std::array;

use dioxus::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierState};
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

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

fn compose_signals<T: Clone + PartialEq, const N: usize>(
    input_signals: &[Signal<T>; N],
    output_signal: &mut Signal<[T; N]>,
    default: impl Fn() -> T,
) {
    let mut new_output = array::from_fn(|_| default());

    for (index, attribute_signal) in input_signals.iter().enumerate() {
        new_output[index] = attribute_signal();
    }

    output_signal.set(new_output);
}

#[component]
fn AttributesForm(attributes: Signal<[skill::Attribute; DICE_PER_SKILL_CHECK]>) -> Element {
    let attribute_signals: [Signal<skill::Attribute>; DICE_PER_SKILL_CHECK] =
        array::from_fn(|index| use_signal(|| attributes()[index]));

    use_effect(move || {
        compose_signals(&attribute_signals, &mut attributes, || {
            skill::Attribute::new(0)
        })
    });

    rsx! {
        for index in 0..DICE_PER_SKILL_CHECK {
            AttributeInput {
                attribute: attribute_signals[index],
                key: "attribute-{index}",
            }
        }
    }
}

#[component]
fn RollsForm(rolls: Signal<[Option<Roll>; DICE_PER_SKILL_CHECK]>) -> Element {
    let roll_signals: [Signal<Option<Roll>>; DICE_PER_SKILL_CHECK] =
        array::from_fn(|index| use_signal(|| rolls()[index]));

    use_effect(move || compose_signals(&roll_signals, &mut rolls, || None));

    rsx! {
        for index in 0..DICE_PER_SKILL_CHECK {
            RollInput {
                roll: roll_signals[index],
                key: "roll-{index}",
            }
        }
    }
}

#[component]
pub fn SkillCheckStateForm(skill_check_state: Signal<PartialSkillCheckState>) -> Element {
    let attributes_signal = use_signal(|| skill_check_state().attributes);
    let skill_value_signal = use_signal(|| skill_check_state().skill_value);
    let rolls_signal = use_signal(|| skill_check_state().fixed_rolls);
    let fate_point_signal =
        use_signal(|| skill_check_state().modifiers.count_of(Modifier::FatePoint));
    let aptitude_signal = use_signal(|| get_aptitude(&skill_check_state()));

    use_effect(move || {
        let mut modifiers = ModifierState::default();

        for _ in 0..fate_point_signal() {
            modifiers.add(Modifier::FatePoint);
        }

        if let Some(aptitude) = aptitude_signal() {
            modifiers.add(Modifier::Aptitude(aptitude));
        }

        skill_check_state.set(PartialSkillCheckState {
            attributes: attributes_signal(),
            roll_caps: [None; DICE_PER_SKILL_CHECK],
            fixed_rolls: rolls_signal(),
            skill_value: skill_value_signal(),
            extra_quality_levels_on_success: None,
            extra_skill_points_on_success: SkillPoints::new(0),
            modifiers,
            inaptitude: false,
        });
    });

    rsx! {
        div {
            class: "skill-check-state-form center-box",

            "Attributes",

            AttributesForm {
                attributes: attributes_signal,
            },

            "Skill Value",

            SkillPointsInput {
                skill_points: skill_value_signal,
            },

            FlexBreak {},

            "Roll",

            RollsForm {
                rolls: rolls_signal,
            },

            FlexBreak {},

            // TODO adapt the UI to represent the domain model more (list of modifiers)

            "Fate Points",

            FatePointInput {
                fate_point_count: fate_point_signal,
            },

            "Aptitude Dice",

            AptitudeInput {
                aptitude: aptitude_signal,
            }
        }
    }
}
