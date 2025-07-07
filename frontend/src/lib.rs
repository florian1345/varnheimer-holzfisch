use dioxus::prelude::*;
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::skill;
use scholle::ScholleEvaluator;
use scholle::error::ScholleError;

use crate::components::evaluation_outcome_view::EvaluationOutcomeView;
use crate::components::scholle_input::ScholleInput;
use crate::components::skill_check_state::SkillCheckStateForm;
use crate::evaluate::EvaluationOutcome;

mod components;
pub(crate) mod display;
mod evaluate;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.scss");
const HEADER_SVG: Asset = asset!("/assets/varnheimer-holzfisch.svg");
const MATERIAL_ICONS: &str = "https://fonts.googleapis.com/icon?family=Material+Icons";

pub const DEFAULT_ATTRIBUTE: skill::Attribute = skill::Attribute::new(10);

pub fn default_skill_check_state() -> PartialSkillCheckState {
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
pub fn App() -> Element {
    let skill_check_state_signal = use_signal(default_skill_check_state);
    let evaluator = use_signal(|| Some(ScholleEvaluator::new(DEFAULT_SCHOLLE_CODE).unwrap()));
    let mut evaluation_outcome_signal: Signal<Option<EvaluationOutcome>> = use_signal(|| None);
    let mut error: Signal<Option<ScholleError>> = use_signal(|| None);

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: MATERIAL_ICONS }

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
            evaluator,
            error,
        }

        div {
            class: "center-box",

            button {
                class: "evaluate-button",
                disabled: evaluator().is_none(),

                onclick: move |_| {
                    let partial_state = &*skill_check_state_signal.read();
                    let Some(evaluator) = evaluator()
                    else {
                        return;
                    };

                    match evaluate::evaluate(partial_state, &evaluator) {
                        Ok(outcome) => {
                            evaluation_outcome_signal.set(Some(outcome));
                            error.set(None);
                        },
                        Err(err) => {
                            evaluation_outcome_signal.set(None);
                            error.set(Some(err.into()));
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
