use std::num::NonZeroUsize;

use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

#[derive(PartialEq, Clone, Props)]
pub struct RollInputProps {
    roll: Option<Roll>,
    onchange: EventHandler<Option<Roll>>,
}

#[component]
pub fn RollInput(props: RollInputProps) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "20",
            value: props.roll.map(Roll::as_u8).unwrap_or(0),
            onchange: move |event|
                props.onchange.call(Roll::new(event.parsed::<u8>().unwrap())),
        }
    }
}

#[derive(PartialEq, Clone, Props)]
pub struct AttributeInputProps {
    attribute: skill::Attribute,
    onchange: EventHandler<skill::Attribute>,
}

#[component]
pub fn AttributeInput(props: AttributeInputProps) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "20",
            value: props.attribute.as_i32(),
            onchange: move |event|
                props.onchange.call(skill::Attribute::new(event.parsed::<i32>().unwrap())),
        }
    }
}

#[derive(PartialEq, Clone, Props)]
pub struct SkillPointsInputProps {
    skill_points: SkillPoints,
    onchange: EventHandler<SkillPoints>,
}

#[component]
pub fn SkillPointsInput(props: SkillPointsInputProps) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "100",
            value: props.skill_points.as_i32(),
            onchange: move |event|
                props.onchange.call(SkillPoints::new(event.parsed::<i32>().unwrap())),
        }
    }
}

#[derive(PartialEq, Clone, Props)]
pub struct FatePointInputProps {
    fate_point_count: usize,
    onchange: EventHandler<usize>,
}

#[component]
pub fn FatePointInput(props: FatePointInputProps) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "6",
            value: props.fate_point_count,
            onchange: move |event|
                props.onchange.call(event.parsed::<usize>().unwrap()),
        }
    }
}

#[derive(PartialEq, Clone, Props)]
pub struct AptitudeInputProps {
    aptitude: Option<Aptitude>,
    onchange: EventHandler<Option<Aptitude>>,
}

#[component]
pub fn AptitudeInput(props: AptitudeInputProps) -> Element {
    rsx! {
        input {
            type: "number",
            min: "0",
            max: "2",
            value: props.aptitude.map(Aptitude::max_dice).map(NonZeroUsize::get).unwrap_or(0),
            onchange: move |event|
                props.onchange.call(Aptitude::new(event.parsed::<usize>().unwrap()))
        }
    }
}
