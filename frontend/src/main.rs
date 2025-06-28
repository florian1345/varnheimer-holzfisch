mod components;
pub(crate) mod display;
mod evaluate;

use dioxus::prelude::*;
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::skill;
use scholle::ScholleEvaluator;
use scholle::error::ScholleError;

use crate::components::evaluation_outcome_view::EvaluationOutcomeView;
use crate::components::scholle_input::ScholleInput;
use crate::components::skill_check_state::SkillCheckStateForm;
use crate::evaluate::EvaluationOutcome;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.scss");
const HEADER_SVG: Asset = asset!("/assets/varnheimer-holzfisch.svg");

fn main() {
    launch(App);
}

const DEFAULT_ATTRIBUTE: skill::Attribute = skill::Attribute::new(10);

fn default_skill_check_state() -> PartialSkillCheckState {
    PartialSkillCheckState {
        attributes: [DEFAULT_ATTRIBUTE; DICE_PER_SKILL_CHECK],
        roll_caps: [None; DICE_PER_SKILL_CHECK],
        fixed_rolls: [None; DICE_PER_SKILL_CHECK],
        skill_value: Default::default(),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: Default::default(),
        modifiers: Default::default(),
        inaptitude: false,
    }
}

pub const DEFAULT_SCHOLLE_CODE: &str = "quality_level";

#[component]
fn App() -> Element {
    let skill_check_state_signal = use_signal(default_skill_check_state);
    let mut evaluator_signal = use_signal(|| ScholleEvaluator::new(DEFAULT_SCHOLLE_CODE).unwrap());
    let mut evaluation_outcome_signal: Signal<Option<EvaluationOutcome>> = use_signal(|| None);
    let mut error_signal: Signal<Option<ScholleError>> = use_signal(|| None);

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        div {
            class: "header",
            "Varnheimer",
            img { src: HEADER_SVG },
            "Holzfisch"
        }

        SkillCheckStateForm {
            skill_check_state: skill_check_state_signal,
        }

        ScholleInput {
            onnewevaluator: move |new_evaluator| evaluator_signal.set(new_evaluator),
            error_signal,
        }

        div {
            class: "center-box",

            button {
                class: "evaluate-button",

                onclick: move |_| {
                    let partial_state = &*skill_check_state_signal.read();
                    let evaluator = &*evaluator_signal.read();

                    match evaluate::evaluate(partial_state, evaluator) {
                        Ok(outcome) => {
                            evaluation_outcome_signal.set(Some(outcome));
                            error_signal.set(None);
                        },
                        Err(err) => {
                            evaluation_outcome_signal.set(None);
                            error_signal.set(Some(err.into()));
                        }
                    }
                },

                "Evaluate"
            }
        }

        if let Some(evaluation_outcome) = evaluation_outcome_signal.read().as_ref() {
            EvaluationOutcomeView {
                evaluation_outcome: evaluation_outcome.clone(),
            }
        }
    }
}
