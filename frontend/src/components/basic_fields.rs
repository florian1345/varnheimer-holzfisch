use std::num::NonZeroUsize;

use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

#[component]
pub fn RollInput(roll: Signal<Option<Roll>>) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "20",
            value: roll().map(Roll::as_u8).unwrap_or(0),
            onchange: move |event|
                roll.set(Roll::new(event.parsed::<u8>().unwrap())),
        }
    }
}

#[component]
pub fn AttributeInput(attribute: Signal<skill::Attribute>) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "20",
            value: attribute().as_i32(),
            onchange: move |event|
                attribute.set(skill::Attribute::new(event.parsed::<i32>().unwrap())),
        }
    }
}

#[component]
pub fn SkillPointsInput(skill_points: Signal<SkillPoints>) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "100",
            value: skill_points().as_i32(),
            onchange: move |event|
                skill_points.set(SkillPoints::new(event.parsed::<i32>().unwrap())),
        }
    }
}

#[component]
pub fn FatePointInput(fate_point_count: Signal<usize>) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "6",
            value: fate_point_count(),
            onchange: move |event|
                fate_point_count.set(event.parsed::<usize>().unwrap()),
        }
    }
}

#[component]
pub fn AptitudeInput(aptitude: Signal<Option<Aptitude>>) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "2",
            value: aptitude().map(Aptitude::max_dice).map(NonZeroUsize::get).unwrap_or(0),
            onchange: move |event|
                aptitude.set(Aptitude::new(event.parsed::<usize>().unwrap()))
        }
    }
}
