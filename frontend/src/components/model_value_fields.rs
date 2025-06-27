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

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{EventType, FocusEventType, FormEventType, TestFormData};
    use dioxus_test_utils::signal::TestSignal;
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    macro_rules! gen_mount {
        ($fn_name:ident($param_name:ident: $param_type:ty) for $component_name:ident) => {
            fn $fn_name($param_name: TestSignal<$param_type>) -> TestDom {
                #[component]
                fn Wrapper($param_name: TestSignal<$param_type>) -> Element {
                    rsx! {
                        $component_name {
                            $param_name: $param_name.into_signal(),
                        }
                    }
                }

                TestDom::new_with_props(Wrapper, WrapperProps { $param_name })
            }
        };
    }

    gen_mount!(mount_roll_input(roll: Option<Roll>) for RollInput);
    gen_mount!(mount_attribute_input(attribute: skill::Attribute) for AttributeInput);
    gen_mount!(mount_skill_points_input(skill_points: SkillPoints) for SkillPointsInput);
    gen_mount!(mount_fate_point_input(fate_point_count: usize) for FatePointInput);
    gen_mount!(mount_aptitude_input(aptitude: Option<Aptitude>) for AptitudeInput);

    fn enter_in_input(dom: &mut TestDom, input: &str) {
        FormEventType::Input
            .at(dom.find("input"))
            .with(TestFormData {
                value: input.to_owned(),
                ..Default::default()
            })
            .raise(dom);
    }

    #[rstest]
    #[case(None, "")]
    #[case(Roll::new(1), "1")]
    #[case(Roll::new(20), "20")]
    fn roll_input_initial_value(#[case] value: Option<Roll>, #[case] expected_content: &str) {
        let dom = mount_roll_input(TestSignal::new(value));
        assert_that!(dom.find("input")).has_value(expected_content);
    }

    #[rstest]
    #[case("-1", "", None)]
    #[case("0", "", None)]
    #[case("", "", None)]
    #[case("1", "1", Roll::new(1))]
    #[case("20", "20", Roll::new(20))]
    #[case("21", "20", Roll::new(20))]
    fn roll_input_input(
        #[case] input: &str,
        #[case] expected_value: &str,
        #[case] expected_outcome: Option<Roll>,
    ) {
        let (signal, validator) = TestSignal::with_validator(Roll::new(10));
        let mut dom = mount_roll_input(signal);

        enter_in_input(&mut dom, input);
        assert_that!(validator.get()).is_equal_to(expected_outcome);

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);
        assert_that!(dom.find("input")).has_value(expected_value);
    }

    #[test]
    fn attribute_input_initial_value() {
        let dom = mount_attribute_input(TestSignal::new(skill::Attribute::new(5)));
        assert_that!(dom.find("input")).has_value("5");
    }

    #[rstest]
    #[case("-1", "0", skill::Attribute::new(0))]
    #[case("5", "5", skill::Attribute::new(5))]
    #[case("21", "20", skill::Attribute::new(20))]
    fn attribute_input_input(
        #[case] input: &str,
        #[case] expected_value: &str,
        #[case] expected_outcome: skill::Attribute,
    ) {
        let (signal, validator) = TestSignal::with_validator(skill::Attribute::new(10));
        let mut dom = mount_attribute_input(signal);

        enter_in_input(&mut dom, input);
        assert_that!(validator.get()).is_equal_to(expected_outcome);

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);
        assert_that!(dom.find("input")).has_value(expected_value);
    }

    #[test]
    fn skill_points_input_initial_value() {
        let dom = mount_skill_points_input(TestSignal::new(SkillPoints::new(5)));
        assert_that!(dom.find("input")).has_value("5");
    }

    #[rstest]
    #[case("-1", "0", SkillPoints::new(0))]
    #[case("5", "5", SkillPoints::new(5))]
    #[case("101", "100", SkillPoints::new(100))]
    fn skill_points_input_input(
        #[case] input: &str,
        #[case] expected_value: &str,
        #[case] expected_outcome: SkillPoints,
    ) {
        let (signal, validator) = TestSignal::with_validator(SkillPoints::new(10));
        let mut dom = mount_skill_points_input(signal);

        enter_in_input(&mut dom, input);
        assert_that!(validator.get()).is_equal_to(expected_outcome);

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);
        assert_that!(dom.find("input")).has_value(expected_value);
    }

    #[rstest]
    #[case("-1", 0)]
    #[case("5", 5)]
    #[case("7", 6)]
    fn fate_point_input_input(#[case] input: &str, #[case] expected_outcome: usize) {
        let (signal, validator) = TestSignal::with_validator(3);
        let mut dom = mount_fate_point_input(signal);

        enter_in_input(&mut dom, input);
        assert_that!(validator.get()).is_equal_to(expected_outcome);

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);
        assert_that!(dom.find("input")).has_value(expected_outcome.to_string());
    }

    #[rstest]
    #[case(None, "0")]
    #[case(Aptitude::new(1), "1")]
    #[case(Aptitude::new(2), "2")]
    fn aptitude_input_initial_value(
        #[case] value: Option<Aptitude>,
        #[case] expected_content: &str,
    ) {
        let dom = mount_aptitude_input(TestSignal::new(value));
        assert_that!(dom.find("input")).has_value(expected_content);
    }

    #[rstest]
    #[case("-1", "0", None)]
    #[case("0", "0", None)]
    #[case("", "0", None)]
    #[case("2", "2", Aptitude::new(2))]
    #[case("3", "2", Aptitude::new(2))]
    fn aptitude_input_input(
        #[case] input: &str,
        #[case] expected_value: &str,
        #[case] expected_outcome: Option<Aptitude>,
    ) {
        let (signal, validator) = TestSignal::with_validator(Aptitude::new(1));
        let mut dom = mount_aptitude_input(signal);

        enter_in_input(&mut dom, input);
        assert_that!(validator.get()).is_equal_to(expected_outcome);

        FocusEventType::Blur.at(dom.find("input")).raise(&mut dom);
        assert_that!(dom.find("input")).has_value(expected_value);
    }
}
