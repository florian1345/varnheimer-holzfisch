use std::fmt::Display;
use std::num::NonZeroUsize;
use std::str::FromStr;

use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

pub trait Int:
    Clone + Copy + Display + Eq + FromStr + Ord + PartialEq + PartialOrd + 'static
{
    fn is_zero(self) -> bool;

    fn saturating_dec(self) -> Self;

    fn saturating_inc(self) -> Self;
}

macro_rules! impl_int {
    ($typ:ty) => {
        impl Int for $typ {
            fn is_zero(self) -> bool {
                self == 0
            }

            fn saturating_dec(self) -> Self {
                self.saturating_sub(1)
            }

            fn saturating_inc(self) -> Self {
                self.saturating_add(1)
            }
        }
    };
}

impl_int!(u8);
impl_int!(usize);
impl_int!(i32);

fn clamp<T: Int>(mut value: T, min: Option<T>, max: Option<T>) -> T {
    value = min.map(|min| value.max(min)).unwrap_or(value);
    value = max.map(|max| value.min(max)).unwrap_or(value);
    value
}

fn to_string<T: Int>(value: T, zero_as_empty: bool) -> String {
    if zero_as_empty && value.is_zero() {
        String::new()
    }
    else {
        value.to_string()
    }
}

#[component]
pub fn NumberInput<T: Int>(
    value: Signal<T>,
    min: Option<T>,
    max: Option<T>,
    class: Option<String>,
    #[props(default = false)] zero_as_empty: bool,
) -> Element {
    let mut text_value = use_signal(|| to_string(value(), zero_as_empty));

    use_effect(move || {
        let mut text_value = text_value();

        if text_value.is_empty() {
            text_value.push('0');
        }

        if let Ok(parsed_value) = text_value.parse::<T>() {
            value.set(clamp(parsed_value, min, max));
        }
    });

    rsx! {
        div {
            class: {
                if let Some(class) = class {
                    format!("number-input {class}")
                }
                else {
                    "number-input".to_owned()
                }
            },

            input {
                onblur: move |_| {
                    text_value.set(to_string(value(), zero_as_empty));
                },

                oninput: move |event| {
                    text_value.set(event.value());
                },

                value: text_value,
            }

            span {
                class: "number-input-buttons",

                div {
                    button {
                        onclick: move |_| {
                            // Sets value in use_effect
                            let new_value = clamp(value().saturating_inc(), min, max);
                            text_value.set(to_string(new_value, zero_as_empty));
                        },

                        "⯅"
                    }

                    button {
                        onclick: move |_| {
                            // Sets value in use_effect
                            let new_value = clamp(value().saturating_dec(), min, max);
                            text_value.set(to_string(new_value, zero_as_empty));
                        },

                        "⯆"
                    }
                }
            }
        }
    }
}

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

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{FocusEventType, FormEventType, MouseEventType, TestFormData};
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom};
    use kernal::prelude::*;

    use super::*;

    fn mount_number_input<T: Int>(
        value: T,
        min: Option<T>,
        max: Option<T>,
        class: Option<String>,
        zero_as_empty: bool,
    ) -> TestDom {
        #[component]
        fn Wrapper<T: Int>(
            value: T,
            min: Option<T>,
            max: Option<T>,
            class: Option<String>,
            zero_as_empty: bool,
        ) -> Element {
            let value = use_signal(|| value);

            rsx! {
                NumberInput::<T> {
                    value: value,
                    min: min,
                    max: max,
                    class: class,
                    zero_as_empty: zero_as_empty,
                }
            }
        }

        TestDom::new_with_props(
            Wrapper::<T>,
            WrapperProps {
                value,
                min,
                max,
                class,
                zero_as_empty,
            },
        )
    }

    #[test]
    fn number_input_increment_button() {
        let mut dom = mount_number_input(0usize, None, None, None, false);

        assert_that!(dom.find("input")).has_attribute("value", "0");

        dom.find_all("button")[0].trigger(MouseEventType::Click);
        dom.update();

        assert_that!(dom.find("input")).has_attribute("value", "1");
    }

    #[test]
    fn number_input_text_input() {
        let mut dom = mount_number_input(0usize, None, None, None, false);

        dom.find("input").trigger_with(
            FormEventType::Input,
            TestFormData {
                value: "123".to_owned(),
                ..Default::default()
            },
        );
        dom.update();
        dom.find("input").trigger(FocusEventType::Blur);
        dom.update();

        assert_that!(dom.find("input")).has_attribute("value", "123");
    }
}
