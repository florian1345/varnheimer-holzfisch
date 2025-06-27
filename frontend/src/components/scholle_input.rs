use dioxus::prelude::*;
use scholle::ScholleEvaluator;

use crate::DEFAULT_SCHOLLE_CODE;

const SCHOLLE_DOCUMENTATION_LINK: &str =
    "https://github.com/florian1345/varnheimer-holzfisch/blob/master/scholle/README.md";

#[component]
pub fn ScholleInput(onnewevaluator: EventHandler<ScholleEvaluator>) -> Element {
    let mut code_signal = use_signal(|| DEFAULT_SCHOLLE_CODE.to_owned());
    let mut error_signal: Signal<Option<String>> = use_signal(|| None);

    rsx! {
        div {
            class: "scholle-input center-box",

            textarea {
                rows: 12,

                onchange: move |event| {
                    let code = event.value();
                    code_signal.set(code.clone());

                    match ScholleEvaluator::new(code) {
                        Ok(evaluator) => {
                            error_signal.set(None);
                            onnewevaluator.call(evaluator)
                        },
                        Err(e) => error_signal.set(Some(e.to_string()))
                    };
                },

                { format!("{}", code_signal.read()) }
            }

            div {
                class: "help",

                a {
                    href: SCHOLLE_DOCUMENTATION_LINK,

                    "?"
                }
            }
        }

        if let Some(error) = error_signal.read().as_ref().cloned() {
            div {
                class: "error center-box",

                { error }
            }
        }
    }
}
