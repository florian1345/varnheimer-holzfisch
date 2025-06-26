use std::num::NonZeroUsize;

use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

use crate::components::number_input::NumberInput;

#[component]
pub fn RollInput(roll: Signal<Option<Roll>>) -> Element {
    let roll_number = use_signal(|| roll().map(Roll::as_u8).unwrap_or(0));

    use_effect(move || roll.set(Roll::new(roll_number())));

    rsx! {
        NumberInput::<u8> {
            class: "small-input",
            min: 0,
            max: 20,
            value: roll_number,
            zero_as_empty: true,
        }
    }
}

#[component]
pub fn AttributeInput(attribute: Signal<skill::Attribute>) -> Element {
    let attribute_number = use_signal(|| attribute().as_i32());

    use_effect(move || attribute.set(skill::Attribute::new(attribute_number())));

    rsx! {
        NumberInput::<i32> {
            class: "small-input",
            min: 0,
            max: 20,
            value: attribute_number,
        }
    }
}

#[component]
pub fn SkillPointsInput(skill_points: Signal<SkillPoints>) -> Element {
    let skill_points_number = use_signal(|| skill_points().as_i32());

    use_effect(move || skill_points.set(SkillPoints::new(skill_points_number())));

    rsx! {
        NumberInput::<i32> {
            class: "small-input",
            min: 0,
            max: 100,
            value: skill_points_number,
        }
    }
}

#[component]
pub fn FatePointInput(fate_point_count: Signal<usize>) -> Element {
    rsx! {
        NumberInput::<usize> {
            class: "small-input",
            min: 0,
            max: 6,
            value: fate_point_count,
        }
    }
}

#[component]
pub fn AptitudeInput(aptitude: Signal<Option<Aptitude>>) -> Element {
    let aptitude_number = use_signal(|| {
        aptitude()
            .map(Aptitude::max_dice)
            .map(NonZeroUsize::get)
            .unwrap_or(0)
    });

    use_effect(move || aptitude.set(Aptitude::new(aptitude_number())));

    rsx! {
        NumberInput::<usize> {
            class: "small-input",
            min: 0,
            max: 2,
            value: aptitude_number,
        }
    }
}
