use dioxus::prelude::*;

#[component]
pub fn FlexSpacer() -> Element {
    rsx! {
        div {
            class: "flex-spacer",
        }
    }
}
