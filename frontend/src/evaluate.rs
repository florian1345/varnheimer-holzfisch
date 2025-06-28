use model::check::outcome::SkillCheckOutcomeProbabilities;
use model::check::{PartialSkillCheckState, SkillCheckAction};
use model::engine::VarnheimerHolzfischEngine;
use model::evaluation::Evaluated;
use scholle::ScholleEvaluator;
use scholle::error::RuntimeResult;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EvaluationOutcome {
    pub(crate) recommended_action: Option<SkillCheckAction>,
    pub(crate) evaluated_probabilities: Evaluated<SkillCheckOutcomeProbabilities>,
}

pub(crate) fn evaluate(
    partial_state: &PartialSkillCheckState,
    evaluator: &ScholleEvaluator,
) -> RuntimeResult<EvaluationOutcome> {
    let mut engine = VarnheimerHolzfischEngine {
        evaluator: evaluator.clone(),
    };

    if let Some(state) = partial_state.as_skill_check_state() {
        let evaluated_actions = engine.evaluate_all_actions(state)?;
        let (action, evaluated) = evaluated_actions.into_iter().next().unwrap();

        Ok(EvaluationOutcome {
            recommended_action: Some(action),
            evaluated_probabilities: evaluated,
        })
    }
    else {
        Ok(EvaluationOutcome {
            recommended_action: None,
            evaluated_probabilities: engine.evaluate_partial(partial_state.clone())?,
        })
    }
}
