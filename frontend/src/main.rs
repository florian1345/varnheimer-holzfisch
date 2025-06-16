mod components;

use dioxus::prelude::*;
use model::check::{DICE_PER_SKILL_CHECK, PartialSkillCheckState};
use model::skill;
use scholle::ScholleEvaluator;

use crate::components::scholle_input::ScholleInput;
use crate::components::skill_check_state::SkillCheckStateForm;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
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

pub const DEFAULT_SCHOLLE_CODE: &str =
    "(if is_success then quality_level else -1) + 2 * remaining_fate_points";

#[component]
fn App() -> Element {
    let mut skill_check_state_signal = use_signal(default_skill_check_state);
    let mut evaluator_result_signal =
        use_signal(|| ScholleEvaluator::new(DEFAULT_SCHOLLE_CODE).unwrap());

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
            skill_check_state: skill_check_state_signal.read().clone(),
            onchange: move |new_state| skill_check_state_signal.set(new_state),
        }

        ScholleInput {
            onnewevaluator: move |new_evaluator| evaluator_result_signal.set(new_evaluator),
        }
    }
}
