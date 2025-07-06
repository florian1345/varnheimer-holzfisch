use dioxus::prelude::*;

#[component]
pub fn Checkbox(checked: bool, label: String, on_change: EventHandler<bool>) -> Element {
    rsx! {
        div {
            class: "checkbox",

            div {
                class: "checkbox-box",
                onclick: move |_| on_change(!checked),

                if checked {
                    div {
                        class: "checkbox-mark"
                    }
                }
            }

            span {
                { label }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{EventReceiver, EventSender, EventType, MouseEventType};
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom, event};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    fn mount(checked: bool, label: impl Into<String>) -> (TestDom, EventReceiver<bool>) {
        #[component]
        fn Wrapper(checked: bool, label: String, on_change: EventSender<bool>) -> Element {
            rsx! {
                Checkbox {
                    checked: checked,
                    label: label,
                    on_change: on_change.into_event_handler(),
                }
            }
        }

        let (on_change, on_change_receiver) = event::event_channel();
        let dom = TestDom::new_with_props(
            Wrapper,
            WrapperProps {
                checked,
                label: label.into(),
                on_change,
            },
        );

        (dom, on_change_receiver)
    }

    #[test]
    fn display_unchecked() {
        let (dom, _) = mount(false, "Test label");

        let checkbox_box = dom.find(".checkbox > .checkbox-box");
        assert_that!(checkbox_box.non_placeholder_children()).is_empty();

        let checkbox_label = dom.find(".checkbox > span");
        assert_that!(checkbox_label).contains_only_text("Test label");
    }

    #[test]
    fn display_checked() {
        let (dom, _) = mount(true, "Test label");

        let box_children = dom.find(".checkbox > .checkbox-box").children();
        assert_that!(&box_children).has_length(1);
        assert_that!(box_children[0])
            .has_tag("div")
            .has_exactly_classes(["checkbox-mark"]);
    }

    #[rstest]
    #[case::check(false, true)]
    #[case::uncheck(true, false)]
    fn check(#[case] before: bool, #[case] expected_after: bool) {
        let (mut dom, mut on_checked_receiver) = mount(before, "Test label");

        MouseEventType::Click
            .at(dom.find(".checkbox-box"))
            .raise(&mut dom);

        assert_that!(on_checked_receiver.try_next()).contains_value(Some(expected_after));
    }
}
