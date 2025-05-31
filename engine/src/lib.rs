use model::check::{
    SkillCheckAction,
    PartialSkillCheckState,
    SkillCheckState,
    SkillCheckActionResult,
    SkillCheckOutcomeProbabilities
};
use model::engine::SkillCheckEngine;
use model::evaluation::{Evaluated, Evaluation, SkillCheckEvaluator};
use model::probability::Probability;
use model::roll::{Roll, DICE_SIDES};

pub struct VarnheimerHolzfischEngine<EvaluatorT> {
    pub evaluator: EvaluatorT
}

const DIE_RESULT_PROBABILITY: Probability = Probability::new(1.0f64 / DICE_SIDES as f64).unwrap();

impl<EvaluatorT: SkillCheckEvaluator> SkillCheckEngine for VarnheimerHolzfischEngine<EvaluatorT> {
    fn evaluate_action(
        &mut self,
        state: SkillCheckState,
        action: SkillCheckAction
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        match action.apply(state) {
            SkillCheckActionResult::Done(outcome) => {
                let evaluation = self.evaluator.evaluate(outcome);

                Evaluated {
                    evaluated: SkillCheckOutcomeProbabilities::of_known_outcome(outcome),
                    evaluation,
                }
            },
            SkillCheckActionResult::State(state) => self.evaluate(state),
            SkillCheckActionResult::PartialState(partial_state) =>
                self.evaluate_partial(partial_state),
        }
    }

    fn evaluate_partial(
        &mut self,
        mut state: PartialSkillCheckState
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        if let Some(state) = state.as_skill_check_state() {
            return self.evaluate(state);
        }

        let first_unrolled_idx = state.fixed_rolls.iter_mut()
            .enumerate()
            .find(|(_, roll)| roll.is_none())
            .map(|(idx, _)| idx)
            .unwrap();
        let mut probabilities = SkillCheckOutcomeProbabilities::default();
        let mut evaluation = Evaluation::ZERO;

        let rolls_to_analyze = if let Some(cap) = state.roll_caps[first_unrolled_idx] {
            // evaluate rolling >= the cap first, then the rest in the loop below
            let rolls_at_least_cap = Roll::ALL.into_iter()
                .filter(|&roll| roll >= cap)
                .count();
            let probability = DIE_RESULT_PROBABILITY.saturating_mul(rolls_at_least_cap);
            state.fixed_rolls[first_unrolled_idx] = Some(cap);
            let sub_evaluated = self.evaluate_partial(state);
            probabilities.saturating_add_assign(&(sub_evaluated.evaluated.clone() * probability));
            evaluation += sub_evaluated.evaluation * probability;

            Roll::ALL.split(|&roll| roll == cap).next().unwrap()
        }
        else {
            // no cap given, all options are equally likely
            &Roll::ALL
        };

        for &roll in rolls_to_analyze {
            state.fixed_rolls[first_unrolled_idx] = Some(roll);
            let sub_evaluated = self.evaluate_partial(state);
            probabilities.saturating_add_assign(
                &(sub_evaluated.evaluated.clone() * DIE_RESULT_PROBABILITY));
            evaluation += sub_evaluated.evaluation * DIE_RESULT_PROBABILITY;
        }

        Evaluated {
            evaluated: probabilities,
            evaluation
        }
    }
}
