use model::check::{
    SkillCheckAction,
    SkillCheckProbabilities,
    PartialSkillCheckState,
    SkillCheckState
};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluated, Evaluation, SkillCheckEvaluator};
use model::probability::Probability;
use model::roll::{Roll, DICE_SIDES};

pub struct HolzfischEngine<EvaluatorT> {
    pub evaluator: EvaluatorT
}

const DIE_RESULT_PROBABILITY: Probability = Probability::new(1.0f64 / DICE_SIDES as f64).unwrap();

impl<EvaluatorT: SkillCheckEvaluator> SkillCheckEngine for HolzfischEngine<EvaluatorT> {
    fn evaluate_action(
        &mut self,
        state: SkillCheckState,
        action: SkillCheckAction
    ) -> Evaluated<SkillCheckProbabilities> {
        match action {
            SkillCheckAction::Accept => {
                let outcome = state.current_outcome();
                let evaluation = self.evaluator.evaluate(outcome);

                Evaluated {
                    evaluated: SkillCheckProbabilities::of_known_outcome(outcome),
                    evaluation,
                }
            }
        }
    }

    fn evaluate_partial(
        &mut self,
        mut state: PartialSkillCheckState
    ) -> Evaluated<SkillCheckProbabilities> {
        if let Some(state) = state.as_skill_check_state() {
            return self.evaluate(state).into_iter()
                .map(|(_, evaluated)| evaluated)
                .max_by_key(|&evaluated| evaluated.evaluation)
                .unwrap();
        }

        let first_unrolled_idx = state.fixed_rolls.iter_mut()
            .enumerate()
            .find(|(_, roll)| roll.is_none())
            .map(|(idx, _)| idx)
            .unwrap();
        let mut probabilities = SkillCheckProbabilities::default();
        let mut evaluation = Evaluation::ZERO;

        for roll in Roll::ALL {
            state.fixed_rolls[first_unrolled_idx] = Some(roll);
            let sub_evaluated = self.evaluate_partial(state);
            probabilities = probabilities.saturating_add(
                sub_evaluated.evaluated * DIE_RESULT_PROBABILITY);
            evaluation += sub_evaluated.evaluation * DIE_RESULT_PROBABILITY;
        }

        Evaluated {
            evaluated: probabilities,
            evaluation
        }
    }
}
