use dioxus::prelude::*;
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::roll::Roll;
use model::skill;

use crate::components::checkbox::Checkbox;
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

            Checkbox {
                checked: skill_check_state().inaptitude,
                on_change: move |inaptitude| skill_check_state.write().inaptitude = inaptitude,
                label: "Inaptitude",
            }

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

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{
        EventType,
        FocusEventType,
        FormEventType,
        MouseEventType,
        TestFormData,
    };
    use dioxus_test_utils::signal::{TestSignal, TestSignalAccess};
    use dioxus_test_utils::{Find, NodeRef, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use model::check::modifier::{Modifier, ModifierState};
    use model::skill::SkillPoints;
    use rstest::rstest;

    use super::*;
    use crate::{DEFAULT_ATTRIBUTE, default_skill_check_state};

    fn mount(
        skill_check_state: PartialSkillCheckState,
    ) -> (TestDom, TestSignalAccess<PartialSkillCheckState>) {
        #[component]
        fn Wrapper(skill_check_state: TestSignal<PartialSkillCheckState>) -> Element {
            rsx! {
                SkillCheckStateForm {
                    skill_check_state: skill_check_state.into_signal(),
                }
            }
        }

        let (skill_check_state, skill_check_state_access) =
            TestSignal::with_access(skill_check_state);
        let dom = TestDom::new_with_props(Wrapper, WrapperProps { skill_check_state });
        (dom, skill_check_state_access)
    }

    fn get_number_input(dom: &TestDom, index: usize) -> NodeRef {
        dom.find_all(".skill-check-state-form > .number-input > input")[index]
    }

    fn set_number_input(dom: &mut TestDom, index: usize, value: &str) {
        FormEventType::Input
            .at(get_number_input(dom, index))
            .with(TestFormData {
                value: value.to_owned(),
                ..Default::default()
            })
            .raise(dom);
        FocusEventType::Blur
            .at(get_number_input(dom, index))
            .raise(dom);
    }

    #[rstest]
    #[case::first(0, "15", [skill::Attribute::new(15), DEFAULT_ATTRIBUTE, DEFAULT_ATTRIBUTE])]
    #[case::second(1, "21", [DEFAULT_ATTRIBUTE, skill::Attribute::new(20), DEFAULT_ATTRIBUTE])]
    #[case::third(2, "-1", [DEFAULT_ATTRIBUTE, DEFAULT_ATTRIBUTE, skill::Attribute::new(0)])]
    fn input_attribute(
        #[case] index: usize,
        #[case] value: &str,
        #[case] expected_attributes: [skill::Attribute; DICE_PER_SKILL_CHECK],
    ) {
        let (mut dom, skill_check_state_access) = mount(default_skill_check_state());

        set_number_input(&mut dom, index, value);

        assert_that!(skill_check_state_access.get()).is_equal_to(PartialSkillCheckState {
            attributes: expected_attributes,
            ..default_skill_check_state()
        });
        assert_that!(get_number_input(&dom, 0))
            .has_attribute("value", expected_attributes[0].as_i32().to_string());
        assert_that!(get_number_input(&dom, 1))
            .has_attribute("value", expected_attributes[1].as_i32().to_string());
        assert_that!(get_number_input(&dom, 2))
            .has_attribute("value", expected_attributes[2].as_i32().to_string());
    }

    #[test]
    fn input_skill_points() {
        let (mut dom, skill_check_state_access) = mount(default_skill_check_state());

        set_number_input(&mut dom, 3, "13");

        assert_that!(skill_check_state_access.get()).is_equal_to(PartialSkillCheckState {
            skill_value: SkillPoints::new(13),
            ..default_skill_check_state()
        });
        assert_that!(get_number_input(&dom, 3)).has_attribute("value", "13");
    }

    #[rstest]
    #[case::first(0, "15", [Some(Roll::new(15).unwrap()), None, None], ["15", "", ""])]
    #[case::second(1, "21", [None, Some(Roll::new(20).unwrap()), None], ["", "20", ""])]
    #[case::third(2, "1", [None, None, Some(Roll::new(1).unwrap())], ["", "", "1"])]
    #[case::first_to_none(0, "0", [None, None, None], ["", "", ""])]
    #[case::second_to_none(1, "", [None, None, None], ["", "", ""])]
    #[case::third_to_none(2, "-1", [None, None, None], ["", "", ""])]
    fn input_rolls(
        #[case] index: usize,
        #[case] value: &str,
        #[case] expected_fixed_rolls: [Option<Roll>; DICE_PER_SKILL_CHECK],
        #[case] expected_values: [&str; DICE_PER_SKILL_CHECK],
    ) {
        let (mut dom, skill_check_state_access) = mount(default_skill_check_state());

        set_number_input(&mut dom, index + 4, value);

        assert_that!(skill_check_state_access.get()).is_equal_to(PartialSkillCheckState {
            fixed_rolls: expected_fixed_rolls,
            ..default_skill_check_state()
        });
        assert_that!(get_number_input(&dom, 4)).has_attribute("value", expected_values[0]);
        assert_that!(get_number_input(&dom, 5)).has_attribute("value", expected_values[1]);
        assert_that!(get_number_input(&dom, 6)).has_attribute("value", expected_values[2]);
    }

    #[test]
    fn input_modifier() {
        let (mut dom, skill_check_state_access) = mount(default_skill_check_state());

        FormEventType::Change
            .at(dom.find("select"))
            .with(TestFormData {
                value: "fate-point".to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);
        MouseEventType::Click
            .at(dom.find(".end-button"))
            .raise(&mut dom);

        assert_that!(skill_check_state_access.get()).is_equal_to(PartialSkillCheckState {
            modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
            ..default_skill_check_state()
        });
    }

    #[test]
    fn input_inaptitude() {
        let (mut dom, skill_check_state_access) = mount(default_skill_check_state());

        MouseEventType::Click
            .at(dom.find(".checkbox-box"))
            .raise(&mut dom);

        assert_that!(skill_check_state_access.get()).is_equal_to(PartialSkillCheckState {
            inaptitude: true,
            ..default_skill_check_state()
        });
    }
}
