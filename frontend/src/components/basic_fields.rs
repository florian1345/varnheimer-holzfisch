use std::fmt::Display;
use std::num::{IntErrorKind, NonZeroUsize, ParseIntError};
use std::str::FromStr;

use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

pub trait Int: Clone + Copy + Display + Eq + FromStr<Err = ParseIntError> + Ord + 'static {
    const MIN: Self;
    const MAX: Self;

    fn is_zero(self) -> bool;

    fn saturating_dec(self) -> Self;

    fn saturating_inc(self) -> Self;
}

macro_rules! impl_int {
    ($typ:ty) => {
        impl Int for $typ {
            const MIN: $typ = <$typ>::MIN;
            const MAX: $typ = <$typ>::MAX;

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

        let parsed_value = match text_value.parse::<T>() {
            Ok(parsed_value) => Some(parsed_value),
            Err(err) if err.kind() == &IntErrorKind::PosOverflow => Some(T::MAX),
            Err(err) if err.kind() == &IntErrorKind::NegOverflow => Some(T::MIN),
            _ => None,
        };

        if let Some(parsed_value) = parsed_value {
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
    use dioxus_test_utils::event::{
        EventType,
        FocusEventType,
        FormEventType,
        MouseEventType,
        TestFormData,
    };
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use rstest::rstest;

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

    #[rstest]
    #[case::increment_without_max(5, None, "6")]
    #[case::increment_below_max(5, Some(6), "6")]
    #[case::increment_capped_by_max(10, Some(10), "10")]
    #[case::increment_capped_by_max_of_type(255, None, "255")]
    fn number_input_increment_button(
        #[case] initial_value: u8,
        #[case] max: Option<u8>,
        #[case] expected_value: &str,
    ) {
        let mut dom = mount_number_input(initial_value, None, max, None, false);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", initial_value));

        MouseEventType::Click
            .at(dom.find_all("button")[0])
            .raise(&mut dom);

        assert_that!(dom.find("input")).has_attribute("value", expected_value);
    }

    #[rstest]
    #[case::decrement_without_min(0, None, "-1")]
    #[case::decrement_above_min(5, Some(4), "4")]
    #[case::decrement_capped_by_min(-3, Some(-3), "-3")]
    #[case::decrement_capped_by_min_of_type(i32::MIN, None, &i32::MIN.to_string())]
    fn number_input_decrement_button(
        #[case] initial_value: i32,
        #[case] min: Option<i32>,
        #[case] expected_value: &str,
    ) {
        let mut dom = mount_number_input(initial_value, min, None, None, false);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", initial_value));

        MouseEventType::Click
            .at(dom.find_all("button")[1])
            .raise(&mut dom);

        assert_that!(dom.find("input")).has_attribute("value", expected_value);
    }

    #[rstest]
    #[case::entering_same_number(15, None, None, "15", "15")]
    #[case::entering_invalid_number(15, None, None, "a", "15")]
    #[case::without_bounds(0, None, None, "123", "123")]
    #[case::within_bounds(0, Some(-100), Some(100), "-42", "-42")]
    #[case::below_min(0, Some(-100), Some(100), "-123", "-100")]
    #[case::above_max(0, Some(-100), Some(100), "123", "100")]
    #[case::below_min_of_type_without_bounds(0, None, None, "-10000000000", &i32::MIN.to_string())]
    #[case::above_max_of_type_without_bounds(0, None, None, "10000000000", &i32::MAX.to_string())]
    #[case::below_min_of_type_with_bounds(0, Some(-123), None, "-10000000000", "-123")]
    #[case::above_max_of_type_with_bounds(0, None, Some(123), "10000000000", "123")]
    fn number_input_text_input(
        #[case] initial_value: i32,
        #[case] min: Option<i32>,
        #[case] max: Option<i32>,
        #[case] input: &str,
        #[case] expected: &str,
    ) {
        let mut dom = mount_number_input(initial_value, min, max, None, false);

        FormEventType::Input
            .at(dom.find("input"))
            .with(TestFormData {
                value: input.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);
        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);

        assert_that!(dom.find("input")).has_attribute("value", expected);
    }

    #[rstest]
    #[case::yes(true, "")]
    #[case::no(false, "0")]
    fn number_input_zero_as_empty(#[case] zero_as_empty: bool, #[case] expected: &str) {
        let dom = mount_number_input(0usize, None, None, None, zero_as_empty);
        assert_that!(dom.find("input")).has_attribute("value", expected);
    }
}
