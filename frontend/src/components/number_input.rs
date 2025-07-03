use std::fmt::Display;
use std::num::{IntErrorKind, ParseIntError};
use std::str::FromStr;

use dioxus::prelude::*;

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
    value: T,
    on_change: EventHandler<T>,
    min: Option<T>,
    max: Option<T>,
    class: Option<String>,
    #[props(default = false)] zero_as_empty: bool,
) -> Element {
    // Normally input content is derived from `value`, but during editing this can lead to strange
    // behavior, as the text is immediately updated to the model value. For example, attempting to
    // enter "20" in a field with minimum 10 would immediately put a "10" into the field once the
    // "2" is entered. To prevent this, the actual text value during editing is stored in this
    // signal and cleared once editing ends (onblur).
    let mut text_value = use_signal(|| None);

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
                    text_value.set(None);
                },

                oninput: move |event| {
                    let mut value = event.value();

                    text_value.set(Some(value.clone()));

                    if value.is_empty() {
                        value.push('0');
                    }

                    let parsed_value = match value.parse::<T>() {
                        Ok(parsed_value) => Some(parsed_value),
                        Err(err) if err.kind() == &IntErrorKind::PosOverflow => Some(T::MAX),
                        Err(err) if err.kind() == &IntErrorKind::NegOverflow => Some(T::MIN),
                        Err(err) if err.kind() == &IntErrorKind::InvalidDigit => {
                            let mut chars = value.chars();

                            if chars.next() == Some('-')
                                && chars.next().is_some_and(|c| c.is_ascii_digit())
                                && chars.all(|c| c.is_ascii_digit())
                            {
                                // Negative overflow on unsigned integer ('-' is recognized as an invalid digit)
                                Some(T::MIN)
                            }
                            else {
                                None
                            }
                        },
                        _ => None,
                    };

                    if let Some(parsed_value) = parsed_value {
                        on_change.call(clamp(parsed_value, min, max));
                    }
                },

                value: text_value().unwrap_or_else(|| to_string(value, zero_as_empty))
            }

            span {
                class: "number-input-buttons",

                div {
                    button {
                        onclick: move |_| on_change.call(clamp(value.saturating_inc(), min, max)),

                        "⯅"
                    }

                    button {
                        onclick: move |_| on_change.call(clamp(value.saturating_dec(), min, max)),

                        "⯆"
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use dioxus_test_utils::event::{
        EventSender,
        EventType,
        FocusEventType,
        FormEventType,
        MouseEventType,
        TestFormData,
    };
    use dioxus_test_utils::signal::TestSignal;
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom, event};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    fn mount_number_input<T: Int>(
        value: TestSignal<T>,
        on_change: EventSender<T>,
        min: Option<T>,
        max: Option<T>,
        class: Option<String>,
        zero_as_empty: bool,
    ) -> TestDom {
        #[component]
        fn Wrapper<T: Int>(
            value: TestSignal<T>,
            on_change: EventSender<T>,
            min: Option<T>,
            max: Option<T>,
            class: Option<String>,
            zero_as_empty: bool,
        ) -> Element {
            let value = value.into_signal();

            rsx! {
                NumberInput::<T> {
                    value: value(),
                    on_change: on_change.into_event_handler(),
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
                on_change,
                min,
                max,
                class,
                zero_as_empty,
            },
        )
    }

    #[rstest]
    #[case::increment_without_max(5, None, 6)]
    #[case::increment_below_max(5, Some(6), 6)]
    #[case::increment_capped_by_max(10, Some(10), 10)]
    #[case::increment_capped_by_max_of_type(255, None, 255)]
    fn number_input_increment_button(
        #[case] initial_value: u8,
        #[case] max: Option<u8>,
        #[case] expected_value: u8,
    ) {
        let (value_signal, value_access) = TestSignal::with_access(initial_value);
        let (on_changed, mut on_changed_receiver) = event::event_channel();
        let mut dom = mount_number_input(value_signal, on_changed, None, max, None, false);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", initial_value));

        MouseEventType::Click
            .at(dom.find_all("button")[0])
            .raise(&mut dom);

        assert_that!(on_changed_receiver.try_next()).contains_value(Some(expected_value));

        value_access.set(expected_value, &mut dom);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", expected_value));
    }

    #[rstest]
    #[case::decrement_without_min(0, None, -1)]
    #[case::decrement_above_min(5, Some(4), 4)]
    #[case::decrement_capped_by_min(-3, Some(-3), -3)]
    #[case::decrement_capped_by_min_of_type(i32::MIN, None, i32::MIN)]
    fn number_input_decrement_button(
        #[case] initial_value: i32,
        #[case] min: Option<i32>,
        #[case] expected_value: i32,
    ) {
        let (value_signal, value_access) = TestSignal::with_access(initial_value);
        let (on_changed, mut on_changed_receiver) = event::event_channel();
        let mut dom = mount_number_input(value_signal, on_changed, min, None, None, false);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", initial_value));

        MouseEventType::Click
            .at(dom.find_all("button")[1])
            .raise(&mut dom);

        assert_that!(on_changed_receiver.try_next()).contains_value(Some(expected_value));

        value_access.set(expected_value, &mut dom);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", expected_value));
    }

    fn enter_text(dom: &mut TestDom, input: &str) {
        FormEventType::Input
            .at(dom.find("input"))
            .with(TestFormData {
                value: input.to_owned(),
                ..Default::default()
            })
            .raise(dom);
    }

    #[rstest]
    #[case::empty(15, None, None, "", 0)]
    #[case::entering_same_number(15, None, None, "15", 15)]
    #[case::without_bounds(0, None, None, "123", 123)]
    #[case::within_bounds(0, Some(-100), Some(100), "-42", -42)]
    #[case::below_min(0, Some(-100), Some(100), "-123", -100)]
    #[case::above_max(0, Some(-100), Some(100), "123", 100)]
    #[case::below_min_of_type_without_bounds(0, None, None, "-10000000000", i32::MIN)]
    #[case::above_max_of_type_without_bounds(0, None, None, "10000000000", i32::MAX)]
    #[case::below_min_of_unsigned_integer(1u8, None, None, "-1", 0)]
    #[case::below_min_of_type_with_bounds(0, Some(-123), None, "-10000000000", -123)]
    #[case::above_max_of_type_with_bounds(0, None, Some(123), "10000000000", 123)]
    fn number_input_text_input_ok<T: Debug + Int>(
        #[case] initial_value: T,
        #[case] min: Option<T>,
        #[case] max: Option<T>,
        #[case] input: &str,
        #[case] expected_value: T,
    ) {
        let (value_signal, value_access) = TestSignal::with_access(initial_value);
        let (on_changed, mut on_changed_receiver) = event::event_channel();
        let mut dom = mount_number_input(value_signal, on_changed, min, max, None, false);

        enter_text(&mut dom, input);

        assert_that!(dom.find("input")).has_attribute("value", input);
        assert_that!(on_changed_receiver.try_next()).contains_value(Some(expected_value));

        value_access.set(expected_value, &mut dom);
        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);

        assert_that!(dom.find("input")).has_attribute("value", format!("{}", expected_value));
    }

    #[rstest]
    #[case("a")]
    #[case("-a")]
    #[case("-1a")]
    #[case("-")]
    fn number_input_text_input_error(#[case] input: &str) {
        let (on_changed, mut on_changed_receiver) = event::event_channel();
        let mut dom = mount_number_input(TestSignal::new(15), on_changed, None, None, None, false);

        enter_text(&mut dom, input);

        assert_that!(dom.find("input")).has_attribute("value", input);
        assert_that!(on_changed_receiver.try_next()).is_err();

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);

        assert_that!(dom.find("input")).has_attribute("value", "15");
    }

    #[rstest]
    #[case::yes(true, "")]
    #[case::no(false, "0")]
    fn number_input_zero_as_empty(#[case] zero_as_empty: bool, #[case] expected: &str) {
        let (event_sender, _) = event::event_channel();
        let dom = mount_number_input(
            TestSignal::new(0usize),
            event_sender,
            None,
            None,
            None,
            zero_as_empty,
        );
        assert_that!(dom.find("input")).has_attribute("value", expected);
    }
}
