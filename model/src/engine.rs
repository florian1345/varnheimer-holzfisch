use std::collections::{HashMap, HashSet};
use std::mem;
use std::time::Instant;

use crate::check::outcome::SkillCheckOutcomeProbabilities;
use crate::check::{
    PartialSkillCheckState,
    SkillCheckAction,
    SkillCheckActionResult,
    SkillCheckState,
};
use crate::evaluation::{Evaluated, Evaluation, SkillCheckEvaluator};
use crate::probability::Probability;
use crate::roll::{DICE_SIDES, Roll};

const DIE_RESULT_PROBABILITY: Probability = Probability::new(1.0f64 / DICE_SIDES as f64).unwrap();

fn cap_probability(cap: Roll) -> Probability {
    let rolls_at_least_cap = (Roll::MAX.as_u8() - cap.as_u8()) as usize + 1;
    DIE_RESULT_PROBABILITY.saturating_mul(rolls_at_least_cap)
}

#[derive(Clone)]
pub struct VarnheimerHolzfischEngine<EvaluatorT> {
    pub evaluator: EvaluatorT,
}

impl<EvaluatorT> VarnheimerHolzfischEngine<EvaluatorT>
where
    EvaluatorT: SkillCheckEvaluator + Send + Sync,
    EvaluatorT::Error: Send,
{
    pub fn evaluate_partial(
        &self,
        state: PartialSkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        let unevaluated_graph = build_graph(state.clone());
        let evaluated_graph = self.eval_graph(&unevaluated_graph)?;
        Ok(self.eval_partial(state, &evaluated_graph[0]))
    }

    pub fn evaluate_all_actions(
        &self,
        skill_check: SkillCheckState,
    ) -> Result<Vec<(SkillCheckAction, Evaluated<SkillCheckOutcomeProbabilities>)>, EvaluatorT::Error>
    {
        let unevaluated_graph = build_graph_from_initial_states([skill_check.clone()]);
        let evaluated_graph = self.eval_graph(&unevaluated_graph[1..])?;

        // TODO export EvaluatedAction instead of using tuples
        Ok(self
            .eval_state(&skill_check, &evaluated_graph[0])?
            .into_iter()
            .map(|action| (action.action, action.evaluated))
            .collect::<Vec<_>>())
    }

    fn eval_graph(
        &self,
        graph: &[UnevaluatedLayer],
    ) -> Result<Vec<EvaluatedLayer>, EvaluatorT::Error> {
        // TODO only track last layer

        let mut eval_graph = vec![EvaluatedLayer {
            states: HashMap::new(),
        }];

        for layer in graph.iter().rev() {
            // TODO remove measuring
            let before = Instant::now();
            let next_layer = eval_graph.last().unwrap();
            let mut eval_layer = EvaluatedLayer {
                states: HashMap::new(),
            };

            for state in layer.states.iter() {
                let best_action = self
                    .eval_state(state, next_layer)?
                    .into_iter()
                    .next()
                    .unwrap();
                eval_layer.states.insert(state.clone(), best_action);
            }

            let after = Instant::now();

            eprintln!(
                "evaluated layer of size {} in {} s",
                layer.states.len(),
                (after - before).as_secs_f64()
            );

            eval_graph.push(eval_layer);
        }

        eval_graph.reverse();
        Ok(eval_graph)
    }

    fn eval_state(
        &self,
        state: &SkillCheckState,
        next_layer: &EvaluatedLayer,
    ) -> Result<Vec<EvaluatedAction>, EvaluatorT::Error> {
        let mut result = state
            .legal_actions()
            .into_iter()
            .map(|action| {
                let evaluated = match action.apply(state.clone()) {
                    SkillCheckActionResult::Done(outcome) => {
                        let evaluation = self.evaluator.evaluate(&outcome)?;

                        Evaluated {
                            evaluation,
                            evaluated: SkillCheckOutcomeProbabilities::of_known_outcome(outcome),
                        }
                    },
                    SkillCheckActionResult::State(state) => {
                        next_layer.states[&state].evaluated.clone()
                    },
                    SkillCheckActionResult::PartialState(state) => {
                        self.eval_partial(state, next_layer)
                    },
                };

                Ok(EvaluatedAction { action, evaluated })
            })
            .collect::<Result<Vec<_>, _>>()?;

        result.sort_by_key(|evaluated_action| -evaluated_action.evaluated.evaluation);
        Ok(result)
    }

    fn eval_partial(
        &self,
        state: PartialSkillCheckState,
        next_layer: &EvaluatedLayer,
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        let mut evaluation = Evaluation::ZERO;
        let mut probabilities = SkillCheckOutcomeProbabilities::default();

        for (state, prob) in build_states_prob(state) {
            let next_state_evaluated = &next_layer.states[&state].evaluated;

            evaluation += next_state_evaluated.evaluation * prob;
            probabilities.saturating_fma_assign(&next_state_evaluated.evaluated, prob);
        }

        Evaluated {
            evaluation,
            evaluated: probabilities,
        }
    }
}

struct UnevaluatedLayer {
    states: HashSet<SkillCheckState>,
}

#[derive(Clone)]
struct EvaluatedAction {
    action: SkillCheckAction,
    evaluated: Evaluated<SkillCheckOutcomeProbabilities>,
}

struct EvaluatedLayer {
    states: HashMap<SkillCheckState, EvaluatedAction>,
}

struct PartialSkillCheckStateProbabilities(HashMap<PartialSkillCheckState, Probability>);

impl PartialSkillCheckStateProbabilities {
    fn new() -> PartialSkillCheckStateProbabilities {
        PartialSkillCheckStateProbabilities(HashMap::new())
    }

    fn add(&mut self, state: PartialSkillCheckState, probability: Probability) {
        let current_prob = self.0.entry(state).or_insert(Probability::ZERO);
        *current_prob = (*current_prob).saturating_add(probability);
    }

    fn states(&self) -> impl Iterator<Item = &PartialSkillCheckState> {
        self.0.keys()
    }

    fn entries(&self) -> impl Iterator<Item = (&PartialSkillCheckState, Probability)> {
        self.0.iter().map(|(state, &prob)| (state, prob))
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

fn build_graph_from_initial_states(
    states: impl IntoIterator<Item = SkillCheckState>,
) -> Vec<UnevaluatedLayer> {
    let mut graph = vec![UnevaluatedLayer {
        states: states.into_iter().collect(),
    }];

    while !graph.last().unwrap().states.is_empty() {
        let mut next_layer = HashSet::new();

        // TODO remove measuring
        let before = Instant::now();

        for state in &graph.last().unwrap().states {
            let actions = state
                .legal_actions()
                .into_iter()
                .filter(|action| action != &SkillCheckAction::Accept);

            for action in actions {
                match action.apply(state.clone()) {
                    SkillCheckActionResult::State(state) => {
                        next_layer.insert(state);
                    },
                    SkillCheckActionResult::PartialState(state) => {
                        next_layer.extend(build_states(state));
                    },
                    SkillCheckActionResult::Done(_) => unreachable!(
                        "received done action result even though Accept was filtered out"
                    ),
                }
            }
        }

        let after = Instant::now();

        eprintln!(
            "built new layer in {} s. previous size: {}  next size: {}",
            (after - before).as_secs_f64(),
            graph.last().unwrap().states.len(),
            next_layer.len()
        );

        graph.push(UnevaluatedLayer { states: next_layer })
    }

    graph.pop();
    graph
}

fn build_graph(state: PartialSkillCheckState) -> Vec<UnevaluatedLayer> {
    build_graph_from_initial_states(build_states(state))
}

fn build_states(state: PartialSkillCheckState) -> impl Iterator<Item = SkillCheckState> {
    build_states_prob(state).map(|(state, _)| state)
}

fn build_states_prob(
    state: PartialSkillCheckState,
) -> impl Iterator<Item = (SkillCheckState, Probability)> {
    let mut current_states = PartialSkillCheckStateProbabilities::new();
    current_states.add(state, Probability::ONE);
    let mut next_states = PartialSkillCheckStateProbabilities::new();

    while current_states
        .states()
        .any(|state| state.as_skill_check_state().is_none())
    {
        for (state, probability) in current_states.entries() {
            if state.inaptitude && state.fixed_rolls.iter().all(Option::is_some) {
                let mut child_state = state.clone();
                child_state.apply_inaptitude();
                next_states.add(child_state, probability);
                continue;
            }

            let first_unrolled_idx = state.fixed_rolls.iter().position(Option::is_none).unwrap();

            let rolls_to_analyze = if let Some(cap) = state.roll_caps[first_unrolled_idx] {
                // evaluate rolling >= the cap first, then the rest in the loop below
                let roll_probability = cap_probability(cap);
                let mut child_state = state.clone();
                child_state.fixed_rolls[first_unrolled_idx] = Some(cap);
                next_states.add(child_state, probability * roll_probability);

                Roll::ALL.split(|&roll| roll == cap).next().unwrap()
            }
            else {
                // no cap given, all options are equally likely
                &Roll::ALL
            };

            for &roll in rolls_to_analyze {
                let mut child_state = state.clone();
                child_state.fixed_rolls[first_unrolled_idx] = Some(roll);
                next_states.add(child_state, probability * DIE_RESULT_PROBABILITY);
            }
        }

        mem::swap(&mut current_states, &mut next_states);
        next_states.clear();
    }

    current_states
        .entries()
        .map(|(state, probability)| (state.as_skill_check_state().unwrap(), probability))
        .collect::<Vec<_>>()
        .into_iter()
}
