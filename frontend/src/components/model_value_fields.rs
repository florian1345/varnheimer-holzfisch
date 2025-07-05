use dioxus::prelude::*;
use model::check::modifier::Aptitude;
use model::roll::Roll;
use model::skill;
use model::skill::SkillPoints;

use crate::components::number_input::NumberInput;

#[component]
pub fn RollInput(roll: Option<Roll>, on_change: EventHandler<Option<Roll>>) -> Element {
    rsx! {
        NumberInput::<u8> {
            class: "small-input",
            min: 0,
            max: 20,
            value: roll.map(Roll::as_u8).unwrap_or(0),
            on_change: move |value| on_change(Roll::new(value)),
            zero_as_empty: true,
        }
    }
}

#[component]
pub fn AttributeInput(
    attribute: skill::Attribute,
    on_change: EventHandler<skill::Attribute>,
) -> Element {
    rsx! {
        NumberInput::<i32> {
            class: "small-input",
            min: 0,
            max: 20,
            value: attribute.as_i32(),
            on_change: move |value| on_change(skill::Attribute::new(value)),
        }
    }
}

#[component]
pub fn SkillPointsInput(
    skill_points: SkillPoints,
    on_change: EventHandler<SkillPoints>,
    #[props(default = false)] non_zero: bool,
) -> Element {
    rsx! {
        NumberInput::<i32> {
            class: "small-input",
            min: if non_zero { 1 } else { 0 },
            max: 100,
            value: skill_points.as_i32(),
            on_change: move |value| on_change(SkillPoints::new(value)),
        }
    }
}

#[component]
pub fn AptitudeInput(aptitude: Aptitude, on_change: EventHandler<Aptitude>) -> Element {
    rsx! {
        NumberInput::<usize> {
            class: "small-input",
            min: 1,
            max: 2,
            value: aptitude.max_dice().get(),
            on_change: move |value| on_change(Aptitude::new(value).unwrap()),
        }
    }
}

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{EventSender, EventType, FormEventType, TestFormData};
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom, event};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    macro_rules! gen_mount {
        ($fn_name:ident($param_name:ident: $param_type:ty $(, $extra_prop_name:ident: $extra_prop_type:ty)*) for $component_name:ident) => {
            fn $fn_name(
                $param_name: $param_type,
                on_change: EventSender<$param_type>,
                $($extra_prop_name: $extra_prop_type,)*
            ) -> TestDom {
                #[component]
                fn Wrapper(
                    $param_name: $param_type,
                    on_change: EventSender<$param_type>,
                    $($extra_prop_name: $extra_prop_type,)*
                ) -> Element {
                    rsx! {
                        $component_name {
                            $param_name: $param_name,
                            on_change: on_change.into_event_handler(),
                            $($extra_prop_name: $extra_prop_name,)*
                        }
                    }
                }

                TestDom::new_with_props(
                    Wrapper,
                    WrapperProps {
                        $param_name,
                        on_change,
                        $($extra_prop_name,)*
                    },
                )
            }
        };
    }

    gen_mount!(mount_roll_input(roll: Option<Roll>) for RollInput);
    gen_mount!(mount_attribute_input(attribute: skill::Attribute) for AttributeInput);
    gen_mount!(mount_skill_points_input(skill_points: SkillPoints, non_zero: bool) for SkillPointsInput);
    gen_mount!(mount_aptitude_input(aptitude: Aptitude) for AptitudeInput);

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
    fn roll_input_display(#[case] value: Option<Roll>, #[case] expected_content: &str) {
        let (on_change, _) = event::event_channel();
        let dom = mount_roll_input(value, on_change);
        assert_that!(dom.find("input")).has_value(expected_content);
    }

    #[rstest]
    #[case("-1", None)]
    #[case("0", None)]
    #[case("", None)]
    #[case("1", Roll::new(1))]
    #[case("20", Roll::new(20))]
    #[case("21", Roll::new(20))]
    fn roll_input_input(#[case] input: &str, #[case] expected_outcome: Option<Roll>) {
        let (on_change, mut on_change_receiver) = event::event_channel();
        let mut dom = mount_roll_input(Roll::new(10), on_change);

        enter_in_input(&mut dom, input);
        assert_that!(on_change_receiver.try_next()).contains_value(Some(expected_outcome));
    }

    #[test]
    fn attribute_input_display() {
        let (on_change, _) = event::event_channel();
        let dom = mount_attribute_input(skill::Attribute::new(5), on_change);
        assert_that!(dom.find("input")).has_value("5");
    }

    #[rstest]
    #[case("-1", skill::Attribute::new(0))]
    #[case("5", skill::Attribute::new(5))]
    #[case("21", skill::Attribute::new(20))]
    fn attribute_input_input(#[case] input: &str, #[case] expected_outcome: skill::Attribute) {
        let (on_change, mut on_change_receiver) = event::event_channel();
        let mut dom = mount_attribute_input(skill::Attribute::new(10), on_change);

        enter_in_input(&mut dom, input);
        assert_that!(on_change_receiver.try_next()).contains_value(Some(expected_outcome));
    }

    #[test]
    fn skill_points_input_display() {
        let (on_change, _) = event::event_channel();
        let dom = mount_skill_points_input(SkillPoints::new(5), on_change, false);
        assert_that!(dom.find("input")).has_value("5");
    }

    #[rstest]
    #[case("-1", false, SkillPoints::new(0))]
    #[case("-1", true, SkillPoints::new(1))]
    #[case("0", false, SkillPoints::new(0))]
    #[case("0", true, SkillPoints::new(1))]
    #[case("5", false, SkillPoints::new(5))]
    #[case("5", true, SkillPoints::new(5))]
    #[case("101", false, SkillPoints::new(100))]
    #[case("101", true, SkillPoints::new(100))]
    fn skill_points_input_input(
        #[case] input: &str,
        #[case] non_zero: bool,
        #[case] expected_outcome: SkillPoints,
    ) {
        let (on_change, mut on_change_receiver) = event::event_channel();
        let mut dom = mount_skill_points_input(SkillPoints::new(10), on_change, non_zero);

        enter_in_input(&mut dom, input);
        assert_that!(on_change_receiver.try_next()).contains_value(Some(expected_outcome));
    }

    #[rstest]
    #[case(Aptitude::new(1).unwrap(), "1")]
    #[case(Aptitude::new(2).unwrap(), "2")]
    fn aptitude_input_display(#[case] value: Aptitude, #[case] expected_content: &str) {
        let (on_change, _) = event::event_channel();
        let dom = mount_aptitude_input(value, on_change);
        assert_that!(dom.find("input")).has_value(expected_content);
    }

    #[rstest]
    #[case("-1", Aptitude::new(1).unwrap())]
    #[case("0", Aptitude::new(1).unwrap())]
    #[case("", Aptitude::new(1).unwrap())]
    #[case("2", Aptitude::new(2).unwrap())]
    #[case("3", Aptitude::new(2).unwrap())]
    fn aptitude_input_input(#[case] input: &str, #[case] expected_outcome: Aptitude) {
        let (on_change, mut on_change_receiver) = event::event_channel();
        let mut dom = mount_aptitude_input(Aptitude::new(1).unwrap(), on_change);

        enter_in_input(&mut dom, input);
        assert_that!(on_change_receiver.try_next()).contains_value(Some(expected_outcome));
    }
}
