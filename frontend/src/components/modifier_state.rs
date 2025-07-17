use dioxus::prelude::*;
use model::check::modifier::{Aptitude, Modifier, ModifierState};
use model::skill::SkillPoints;

use crate::components::flex_spacer::FlexSpacer;
use crate::components::model_value_fields::{AptitudeInput, SkillPointsInput};
use crate::components::number_input::NumberInput;

const MODIFIER_ID_FATE_POINT: &str = "fate-point";
const MODIFIER_ID_APTITUDE: &str = "aptitude";
const MODIFIER_ID_EXTRA_SKILL_POINTS: &str = "extra-skill-points";
const MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS: &str = "extra-skill-points-on-success";
const MODIFIER_ID_EXTRA_QUALITY_LEVEL: &str = "extra-quality-level";

fn modifier_from_id(id: &str) -> Option<Modifier> {
    match id {
        MODIFIER_ID_FATE_POINT => Some(Modifier::FatePoint),
        MODIFIER_ID_APTITUDE => Some(Modifier::Aptitude(Aptitude::new(1).unwrap())),
        MODIFIER_ID_EXTRA_SKILL_POINTS => Some(Modifier::ExtraSkillPoints(SkillPoints::new(1))),
        MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS => {
            Some(Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(1)))
        },
        MODIFIER_ID_EXTRA_QUALITY_LEVEL => Some(Modifier::ExtraQualityLevelOnSuccess),
        _ => None,
    }
}

fn modifier_id(modifier: Modifier) -> &'static str {
    match modifier {
        Modifier::FatePoint => MODIFIER_ID_FATE_POINT,
        Modifier::Aptitude(_) => MODIFIER_ID_APTITUDE,
        Modifier::ExtraSkillPoints(_) => MODIFIER_ID_EXTRA_SKILL_POINTS,
        Modifier::ExtraSkillPointsOnSuccess(_) => MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS,
        Modifier::ExtraQualityLevelOnSuccess => MODIFIER_ID_EXTRA_QUALITY_LEVEL,
    }
}

#[component]
fn NewModifierInput(on_added: EventHandler<Modifier>) -> Element {
    let mut modifier = use_signal(|| None);

    rsx! {
        div {
            class: "modifier-row",

            select {
                value: modifier().map(modifier_id).unwrap_or(""),
                onchange: move |event| {
                    modifier.set(Some(modifier_from_id(&event.value()).unwrap()));
                },

                option {
                    hidden: true,
                    value: "",
                    ""
                }

                option {
                    value: MODIFIER_ID_FATE_POINT,
                    "Fate point"
                }

                option {
                    value: MODIFIER_ID_APTITUDE,
                    "Aptitude"
                }

                option {
                    value: MODIFIER_ID_EXTRA_SKILL_POINTS,
                    "Extra skill points"
                }

                option {
                    value: MODIFIER_ID_EXTRA_SKILL_POINTS_ON_SUCCESS,
                    "Extra skill points on success"
                }

                option {
                    value: MODIFIER_ID_EXTRA_QUALITY_LEVEL,
                    "Extra quality level"
                }
            }

            if let Some(Modifier::Aptitude(aptitude)) = modifier() {
                "Dice"

                AptitudeInput {
                    aptitude: aptitude,
                    on_change: move |aptitude| {
                        modifier.set(Some(Modifier::Aptitude(aptitude)));
                    }
                }
            }

            if let Some(Modifier::ExtraSkillPoints(skill_points)) = modifier() {
                "Skill points"

                SkillPointsInput {
                    skill_points: skill_points,
                    on_change: move |skill_points| {
                        modifier.set(Some(Modifier::ExtraSkillPoints(skill_points)));
                    },
                    non_zero: true,
                }
            }

            if let Some(Modifier::ExtraSkillPointsOnSuccess(skill_points)) = modifier() {
                "Skill points"

                SkillPointsInput {
                    skill_points: skill_points,
                    on_change: move |skill_points| {
                        modifier.set(Some(Modifier::ExtraSkillPointsOnSuccess(skill_points)));
                    },
                    non_zero: true,
                }
            }

            FlexSpacer {}

            button {
                class: "end-button",
                disabled: modifier().is_none(),
                onclick: move |_| {
                    if let Some(modifier) = modifier() {
                        on_added(modifier);
                    }

                    modifier.set(None);
                },

                i {
                    class: "material-icons",

                    "add"
                }
            }
        }
    }
}

fn format_modifier(modifier: Modifier) -> String {
    match modifier {
        Modifier::FatePoint => "Fate point".to_owned(),
        Modifier::Aptitude(aptitude) => {
            format!(
                "Aptitude with {} {}",
                aptitude.max_dice().get(),
                if aptitude.max_dice().get() == 1 {
                    "die"
                }
                else {
                    "dice"
                }
            )
        },
        Modifier::ExtraSkillPoints(skill_points) => {
            format!(
                "{} extra skill {}",
                skill_points.as_i32(),
                if skill_points.as_i32() == 1 {
                    "point"
                }
                else {
                    "points"
                }
            )
        },
        Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
            format!(
                "{} extra skill {} on success",
                skill_points.as_i32(),
                if skill_points.as_i32() == 1 {
                    "point"
                }
                else {
                    "points"
                }
            )
        },
        Modifier::ExtraQualityLevelOnSuccess => "Extra quality level on success".to_owned(),
    }
}

fn modifier_key(modifier: Modifier) -> String {
    match modifier {
        Modifier::FatePoint => "fate-point".to_owned(),
        Modifier::Aptitude(aptitude) => {
            format!("aptitude-{}", aptitude.max_dice().get())
        },
        Modifier::ExtraSkillPoints(skill_points) => {
            format!("skill-point-{}", skill_points.as_i32())
        },
        Modifier::ExtraSkillPointsOnSuccess(skill_points) => {
            format!("skill-point-success-{}", skill_points.as_i32())
        },
        Modifier::ExtraQualityLevelOnSuccess => "quality-level".to_owned(),
    }
}

#[component]
fn ModifierRow(modifier: Modifier, count: usize, on_count_change: EventHandler<usize>) -> Element {
    rsx! {
        // TODO This extra div seems to mess up the styling of the trash button (no longer square)
        //  We could move the <hr> into the loop, but that causes all separators to appear at the
        //  bottom for some reason.

        div {
            div {
                class: "modifier-row",

                { format_modifier(modifier) }

                FlexSpacer {}

                "Uses"

                NumberInput::<usize> {
                    class: "small-input",
                    min: 1,
                    value: count,
                    on_change: on_count_change,
                }


                button {
                    class: "end-button",
                    onclick: move |_| on_count_change(0),

                    i {
                        class: "material-icons",

                        "delete"
                    }
                }
            }

            hr {}
        }
    }
}

#[component]
pub fn ModifierStateInput(
    modifier_state: ModifierState,
    on_change: EventHandler<ModifierState>,
) -> Element {
    rsx! {
        div {
            class: "modifier-state-input",

            for modifier in modifier_state.available_modifiers() {
                {
                    let modifier_state = modifier_state.clone();

                    rsx! {
                        ModifierRow {
                            modifier: modifier,
                            count: modifier_state.count_of(modifier),
                            on_count_change: move |target_count| {
                                let mut new_modifier_state = modifier_state.clone();
                                new_modifier_state.set_count(modifier, target_count);
                                on_change(new_modifier_state);
                            },
                            key: "{modifier_key(modifier)}"
                        }
                    }
                }
            }

            NewModifierInput {
                on_added: move |modifier| {
                    let mut new_modifier_state = modifier_state.clone();
                    new_modifier_state.insert(modifier);
                    on_change(new_modifier_state);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{
        EventSender,
        EventType,
        FormEventType,
        MouseEventType,
        TestFormData,
    };
    use dioxus_test_utils::{Find, NodeRef, NodeRefAssertions, TestDom, event};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    fn mount(modifier_state: ModifierState, on_change: EventSender<ModifierState>) -> TestDom {
        #[component]
        fn Wrapper(
            modifier_state: ModifierState,
            on_change: EventSender<ModifierState>,
        ) -> Element {
            rsx! {
                ModifierStateInput {
                    modifier_state: modifier_state,
                    on_change: on_change.into_event_handler(),
                }
            }
        }

        TestDom::new_with_props(
            Wrapper,
            WrapperProps {
                modifier_state,
                on_change,
            },
        )
    }

    fn new_modifier_row(dom: &TestDom) -> NodeRef {
        dom.find_last(".modifier-row").unwrap()
    }

    fn assert_new_modifier_row_is_empty(dom: &TestDom) {
        let new_modifier_row = new_modifier_row(&dom);
        assert_that!(new_modifier_row.find("select")).has_attribute("value", "");
        assert_that!(new_modifier_row.find(".end-button")).has_attribute("disabled", true);
    }

    fn assert_existing_modifier_row(modifier_row: NodeRef, expected_text: &str, uses: usize) {
        assert_that!(modifier_row.children()[0]).is_text(expected_text);
        assert_that!(modifier_row.find(".number-input > input"))
            .has_attribute("value", uses.to_string());
    }

    #[rstest]
    #[case::fate_point(Modifier::FatePoint, 2, "Fate point")]
    #[case::aptitude_1(Modifier::Aptitude(Aptitude::new(1).unwrap()), 1, "Aptitude with 1 die")]
    #[case::aptitude_2(Modifier::Aptitude(Aptitude::new(2).unwrap()), 1, "Aptitude with 2 dice")]
    #[case::extra_skill_points_1(
        Modifier::ExtraSkillPoints(SkillPoints::new(1)),
        3,
        "1 extra skill point"
    )]
    #[case::extra_skill_points_2(
        Modifier::ExtraSkillPoints(SkillPoints::new(2)),
        3,
        "2 extra skill points"
    )]
    #[case::extra_skill_points_on_success_1(
        Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(1)),
        4,
        "1 extra skill point on success"
    )]
    #[case::extra_skill_points_on_success_2(
        Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(2)),
        4,
        "2 extra skill points on success"
    )]
    #[case::extra_quality_level(
        Modifier::ExtraQualityLevelOnSuccess,
        5,
        "Extra quality level on success"
    )]
    fn display_existing_modifier(
        #[case] modifier: Modifier,
        #[case] uses: usize,
        #[case] expected_text: &str,
    ) {
        let (on_change, _) = event::event_channel();
        let dom = mount(
            ModifierState::from_modifiers(vec![modifier; uses]),
            on_change,
        );

        let modifier_rows = dom.find_all(".modifier-row");
        assert_that!(&modifier_rows).has_length(2);

        let modifier_row = modifier_rows[0];
        assert_existing_modifier_row(modifier_row, expected_text, uses);
    }

    #[test]
    fn display_multiple_modifiers() {
        let (on_change, _) = event::event_channel();
        let dom = mount(
            ModifierState::from_modifiers([
                Modifier::FatePoint,
                Modifier::FatePoint,
                Modifier::ExtraQualityLevelOnSuccess,
            ]),
            on_change,
        );

        let modifier_rows = dom.find_all(".modifier-row");
        assert_that!(&modifier_rows).has_length(3);

        assert_existing_modifier_row(modifier_rows[0], "Fate point", 2);
        assert_existing_modifier_row(modifier_rows[1], "Extra quality level on success", 1);
    }

    #[rstest]
    #[case::fate_point("fate-point", Modifier::FatePoint)]
    #[case::extra_quality_level("extra-quality-level", Modifier::ExtraQualityLevelOnSuccess)]
    fn add_modifier_without_param(#[case] id: &str, #[case] modifier: Modifier) {
        let (on_change, mut on_change_access) = event::event_channel();
        let mut dom = mount(ModifierState::default(), on_change);

        let select = new_modifier_row(&dom).find("select");
        FormEventType::Change
            .at(select)
            .with(TestFormData {
                value: id.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);

        assert_that!(new_modifier_row(&dom).try_find(".number-input")).is_none();

        let add_button = new_modifier_row(&dom).find(".end-button");
        MouseEventType::Click.at(add_button).raise(&mut dom);

        assert_that!(on_change_access.try_next())
            .contains_value(Some(ModifierState::from_modifiers([modifier])));
        assert_new_modifier_row_is_empty(&dom);
    }

    #[rstest]
    #[case::aptitude("aptitude", "2", Modifier::Aptitude(Aptitude::new(2).unwrap()))]
    #[case::extra_skill_points(
        "extra-skill-points",
        "3",
        Modifier::ExtraSkillPoints(SkillPoints::new(3))
    )]
    #[case::extra_skill_points_on_success(
        "extra-skill-points-on-success",
        "1",
        Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(1))
    )]
    fn add_modifier_with_param(#[case] id: &str, #[case] param: &str, #[case] modifier: Modifier) {
        let (on_change, mut on_change_access) = event::event_channel();
        let mut dom = mount(ModifierState::default(), on_change);

        let select = new_modifier_row(&dom).find("select");
        FormEventType::Change
            .at(select)
            .with(TestFormData {
                value: id.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);

        let parameter_input = new_modifier_row(&dom).find(".number-input > input");
        FormEventType::Input
            .at(parameter_input)
            .with(TestFormData {
                value: param.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);

        let add_button = new_modifier_row(&dom).find(".end-button");
        MouseEventType::Click.at(add_button).raise(&mut dom);

        assert_that!(on_change_access.try_next())
            .contains_value(Some(ModifierState::from_modifiers([modifier])));
        assert_new_modifier_row_is_empty(&dom);
    }

    #[rstest]
    #[case::same_count(1, "1", 1)]
    #[case::higher_count(1, "2", 2)]
    #[case::lower_count(2, "1", 1)]
    #[case::below_min_1(1, "0", 1)]
    #[case::below_min_2(1, "-1", 1)]
    fn change_uses_of_existing_modifier(
        #[case] count_before: usize,
        #[case] input: &str,
        #[case] count_after: usize,
    ) {
        let (on_change, mut on_change_access) = event::event_channel();
        let mut dom = mount(
            ModifierState::from_modifiers(vec![Modifier::FatePoint; count_before]),
            on_change,
        );

        let uses_input = dom
            .find_first(".modifier-row .number-input > input")
            .unwrap();
        FormEventType::Input
            .at(uses_input)
            .with(TestFormData {
                value: input.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);

        assert_that!(on_change_access.try_next()).contains_value(Some(
            ModifierState::from_modifiers(vec![Modifier::FatePoint; count_after]),
        ));
    }

    #[test]
    fn delete_only_existing_modifier() {
        let (on_change, mut on_change_access) = event::event_channel();
        let mut dom = mount(
            ModifierState::from_modifiers([Modifier::FatePoint; 2]),
            on_change,
        );

        let delete_button = dom.find_first(".modifier-row .end-button").unwrap();
        MouseEventType::Click.at(delete_button).raise(&mut dom);

        assert_that!(on_change_access.try_next()).contains_value(Some(ModifierState::default()));
    }

    #[test]
    fn delete_existing_modifier_among_others() {
        let modifier_1 = Modifier::FatePoint;
        let modifier_2 = Modifier::Aptitude(Aptitude::new(1).unwrap());
        let modifier_3 = Modifier::Aptitude(Aptitude::new(2).unwrap());
        let (on_change, mut on_change_access) = event::event_channel();
        let mut dom = mount(
            ModifierState::from_modifiers([modifier_1, modifier_2, modifier_3]),
            on_change,
        );

        let delete_button_row_2 = dom.find_all(".modifier-row")[1].find(".end-button");
        MouseEventType::Click
            .at(delete_button_row_2)
            .raise(&mut dom);

        assert_that!(on_change_access.try_next()).contains_value(Some(
            ModifierState::from_modifiers([modifier_1, modifier_3]),
        ));
    }
}
