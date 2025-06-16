use dioxus::prelude::*;

#[component]
pub fn FlexBreak() -> Element {
    rsx! {
        div {
            class: "flex-break",
        }
    }
}
