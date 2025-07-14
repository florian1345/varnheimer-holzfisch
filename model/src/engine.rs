use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::{array, mem};

use rayon::prelude::*;

use crate::check::outcome::SkillCheckOutcomeProbabilities;
use crate::check::{
    PartialSkillCheckState,
    SkillCheckAction,
    SkillCheckActionResult,
    SkillCheckState,
};
use crate::evaluation::{Evaluated, SkillCheckEvaluator};
use crate::probability::Probability;
use crate::roll::{DICE_SIDES, Roll};

const DIE_RESULT_PROBABILITY: Probability = Probability::new(1.0f64 / DICE_SIDES as f64).unwrap();

fn cap_probability(cap: Roll) -> Probability {
    let rolls_at_least_cap = (Roll::MAX.as_u8() - cap.as_u8()) as usize + 1;
    DIE_RESULT_PROBABILITY.saturating_mul(rolls_at_least_cap)
}

fn fma_assign(
    lhs: &mut Evaluated<SkillCheckOutcomeProbabilities>,
    rhs: &Evaluated<SkillCheckOutcomeProbabilities>,
    prob: Probability,
) {
    lhs.evaluation += rhs.evaluation * prob;
    lhs.evaluated.saturating_fma_assign(&rhs.evaluated, prob);
}

// TODO reduce amount of cloning required

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
        let unevaluated_graph = build_graph_from_initial_state(state.clone());
        let evaluated_graph = self.eval_graph(unevaluated_graph)?;
        Ok(self.eval_partial_by_resolving_to_complete(state, &evaluated_graph.states))
    }

    pub fn evaluate_all_actions(
        &self,
        skill_check: SkillCheckState,
    ) -> Result<Vec<(SkillCheckAction, Evaluated<SkillCheckOutcomeProbabilities>)>, EvaluatorT::Error>
    {
        let unevaluated_graph = build_graph_from_initial_state(PartialSkillCheckState {
            attributes: skill_check.attributes,
            roll_caps: [None, None, None],
            fixed_rolls: array::from_fn(|i| Some(skill_check.rolls[i])),
            skill_value: skill_check.skill_value,
            extra_quality_levels_on_success: skill_check.extra_quality_levels_on_success,
            extra_skill_points_on_success: skill_check.extra_skill_points_on_success,
            modifiers: skill_check.modifiers.clone(),
            inaptitude: false,
        });
        let evaluated_graph = self.eval_graph(unevaluated_graph.into_iter().skip(1))?;

        // TODO export EvaluatedAction instead of using tuples
        Ok(self
            .eval_state(&skill_check, &evaluated_graph)?
            .into_iter()
            .map(|action| (action.action, action.evaluated))
            .collect::<Vec<_>>())
    }

    fn eval_graph(
        &self,
        graph: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = UnevaluatedLayer>>,
    ) -> Result<EvaluatedLayer, EvaluatorT::Error> {
        let mut current_layer = EvaluatedLayer {
            preceding_partial_states: PartialStateMap::new(),
            states: HashMap::new(),
        };

        for layer in graph.into_iter().rev() {
            let next_states: HashMap<SkillCheckState, EvaluatedAction> = layer
                .states
                .into_par_iter()
                .map(|state| {
                    let best_action = self
                        .eval_state(&state, &current_layer)?
                        .into_iter()
                        .next()
                        .unwrap();
                    Ok((state, best_action))
                })
                .collect::<Result<_, _>>()?;

            let mut next_preceding_partial_states = PartialStateMap::new();
            next_preceding_partial_states
                .maps_by_remaining_steps
                .push(HashMap::new());
            let mut preceding_partial_states_iter = layer
                .preceding_partial_states
                .sets_by_remaining_steps
                .into_iter()
                .skip(1);

            if let Some(partial_layer_1) = preceding_partial_states_iter.next() {
                let eval_layer = partial_layer_1
                    .into_par_iter()
                    .map(|partial_state| {
                        let evaluated = self.eval_partial_by_resolving_to_complete(
                            partial_state.clone(),
                            &next_states,
                        );
                        (partial_state, evaluated)
                    })
                    .collect::<HashMap<_, _>>();

                next_preceding_partial_states
                    .maps_by_remaining_steps
                    .push(eval_layer);
            }

            for partial_layer in preceding_partial_states_iter {
                let eval_layer = partial_layer
                    .into_par_iter()
                    .map(|partial_state| {
                        let evaluated = self.eval_partial_by_resolving_to_children(
                            &partial_state,
                            next_preceding_partial_states
                                .maps_by_remaining_steps
                                .last()
                                .unwrap(),
                        );
                        (partial_state, evaluated)
                    })
                    .collect::<HashMap<_, _>>();
                next_preceding_partial_states
                    .maps_by_remaining_steps
                    .push(eval_layer);
            }

            current_layer = EvaluatedLayer {
                states: next_states,
                preceding_partial_states: next_preceding_partial_states,
            };
        }

        Ok(current_layer)
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
                    SkillCheckActionResult::PartialState(state) => next_layer
                        .preceding_partial_states
                        .get(&state)
                        .unwrap()
                        .clone(),
                };

                Ok(EvaluatedAction { action, evaluated })
            })
            .collect::<Result<Vec<_>, _>>()?;

        result.sort_by_key(|evaluated_action| -evaluated_action.evaluated.evaluation);
        Ok(result)
    }

    fn eval_partial_by_resolving_to_children(
        &self,
        state: &PartialSkillCheckState,
        next_layer_partial_evals: &HashMap<
            PartialSkillCheckState,
            Evaluated<SkillCheckOutcomeProbabilities>,
        >,
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        let mut result = Evaluated::default();

        with_partial_state_children(state, |child_state, prob| {
            fma_assign(&mut result, &next_layer_partial_evals[&child_state], prob);
        });

        result
    }

    fn eval_partial_by_resolving_to_complete(
        &self,
        state: PartialSkillCheckState,
        next_layer_skill_check_state_evals: &HashMap<SkillCheckState, EvaluatedAction>,
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        let mut result = Evaluated::default();

        build_states_prob_with(state, |state, prob| {
            fma_assign(
                &mut result,
                &next_layer_skill_check_state_evals[&state].evaluated,
                prob,
            );
        });

        result
    }
}

fn steps_to_completeness(state: &PartialSkillCheckState) -> usize {
    let remaining_rolls = state
        .fixed_rolls
        .iter()
        .copied()
        .filter(Option::is_none)
        .count();
    let remaining_steps_by_inaptitude = state.inaptitude as usize * 2; // inaptitude - reroll

    remaining_rolls + remaining_steps_by_inaptitude
}

#[derive(Clone)]
struct PartialStateSet {
    sets_by_remaining_steps: Vec<HashSet<PartialSkillCheckState>>,
}

impl PartialStateSet {
    fn new() -> PartialStateSet {
        PartialStateSet {
            sets_by_remaining_steps: Vec::new(),
        }
    }

    fn insert(&mut self, state: PartialSkillCheckState) {
        let index = steps_to_completeness(&state);

        while self.sets_by_remaining_steps.len() <= index {
            self.sets_by_remaining_steps.push(HashSet::new());
        }

        self.sets_by_remaining_steps[index].insert(state);
    }

    fn set_count(&self) -> usize {
        self.sets_by_remaining_steps.len()
    }

    fn borrow_consecutive(
        &mut self,
        lower_index: usize,
    ) -> (
        &mut HashSet<PartialSkillCheckState>,
        &mut HashSet<PartialSkillCheckState>,
    ) {
        let mut iter = self.sets_by_remaining_steps[lower_index..].iter_mut();

        (iter.next().unwrap(), iter.next().unwrap())
    }

    fn extend_with(&mut self, iter: impl IntoIterator<Item = PartialSkillCheckState>) {
        for state in iter {
            self.insert(state);
        }
    }
}

struct PartialStateMap<T> {
    maps_by_remaining_steps: Vec<HashMap<PartialSkillCheckState, T>>,
}

impl<T: Sync> PartialStateMap<T> {
    fn new() -> PartialStateMap<T> {
        PartialStateMap {
            maps_by_remaining_steps: Vec::new(),
        }
    }

    fn get(&self, state: &PartialSkillCheckState) -> Option<&T> {
        let index = steps_to_completeness(state);
        self.maps_by_remaining_steps
            .get(index)
            .and_then(|map| map.get(state))
    }
}

struct UnevaluatedLayer {
    preceding_partial_states: PartialStateSet,
    states: HashSet<SkillCheckState>,
}

#[derive(Clone)]
struct EvaluatedAction {
    action: SkillCheckAction,
    evaluated: Evaluated<SkillCheckOutcomeProbabilities>,
}

struct EvaluatedLayer {
    preceding_partial_states: PartialStateMap<Evaluated<SkillCheckOutcomeProbabilities>>,
    states: HashMap<SkillCheckState, EvaluatedAction>,
}

struct ProbabilityMap<T>(HashMap<T, Probability>);

impl<T: Eq + Hash> ProbabilityMap<T> {
    fn new() -> ProbabilityMap<T> {
        ProbabilityMap(HashMap::new())
    }

    fn add(&mut self, state: T, probability: Probability) {
        let current_prob = self.0.entry(state).or_insert(Probability::ZERO);
        *current_prob = (*current_prob).saturating_add(probability);
    }

    fn entries(&self) -> impl Iterator<Item = (&T, Probability)> {
        self.0.iter().map(|(state, &prob)| (state, prob))
    }

    fn drain(&mut self) -> impl Iterator<Item = (T, Probability)> {
        self.0.drain()
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

fn build_graph_from_initial_state(state: PartialSkillCheckState) -> Vec<UnevaluatedLayer> {
    let preceding_partial_states = build_states_many_into_partial_set([state]);
    let initial_layer = UnevaluatedLayer {
        states: extract_complete_states(&preceding_partial_states).collect(),
        preceding_partial_states,
    };
    let mut graph = vec![initial_layer];

    while !graph.last().unwrap().states.is_empty() {
        let mut next_partial_states = HashSet::new();
        let mut next_states = HashSet::new();

        for state in &graph.last().unwrap().states {
            let actions = state
                .legal_actions()
                .into_iter()
                .filter(|action| action != &SkillCheckAction::Accept);

            for action in actions {
                match action.apply(state.clone()) {
                    SkillCheckActionResult::State(state) => {
                        next_states.insert(state);
                    },
                    SkillCheckActionResult::PartialState(state) => {
                        next_partial_states.insert(state);
                    },
                    SkillCheckActionResult::Done(_) => unreachable!(
                        "received done action result even though Accept was filtered out"
                    ),
                }
            }
        }

        let preceding_partial_states = build_states_many_into_partial_set(next_partial_states);

        next_states.extend(extract_complete_states(&preceding_partial_states));

        graph.push(UnevaluatedLayer {
            states: next_states,
            preceding_partial_states,
        })
    }

    graph.pop();
    graph
}

// TODO reduce duplication of all these build_states_*

/// Calls the given `consumer` all complete states that can result from the given `state` with their
/// respective probability. The consumer may be called multiple times with the same state with the
/// probabilities summing to the correct probability.
fn build_states_prob_with<R>(
    state: PartialSkillCheckState,
    mut consumer: impl FnMut(SkillCheckState, Probability) -> R,
) {
    let remaining_steps = steps_to_completeness(&state);
    let mut current_states = ProbabilityMap::new();
    current_states.add(state, Probability::ONE);
    let mut next_states = ProbabilityMap::new();

    for _ in 0..remaining_steps {
        for (state, probability) in current_states.entries() {
            with_partial_state_children(state, |child_state, child_probability| {
                next_states.add(child_state, child_probability * probability);
            });
        }

        mem::swap(&mut current_states, &mut next_states);
        next_states.clear();
    }

    current_states.drain().for_each(|(state, probability)| {
        consumer(state.as_skill_check_state().unwrap(), probability);
    });
}

fn extract_complete_states(
    partial_state_set: &PartialStateSet,
) -> impl Iterator<Item = SkillCheckState> {
    partial_state_set
        .sets_by_remaining_steps
        .iter()
        .next()
        .into_iter()
        .flat_map(|set| {
            set.iter()
                .map(|state| state.as_skill_check_state().unwrap())
        })
}

fn build_states_many_into_partial_set(
    root_states: impl IntoIterator<Item = PartialSkillCheckState>,
) -> PartialStateSet {
    let mut states = PartialStateSet::new();
    states.extend_with(root_states);

    for lower_index in (0..states.set_count().saturating_sub(1)).rev() {
        let (next_states, current_states) = states.borrow_consecutive(lower_index);

        for state in current_states.iter() {
            with_partial_state_children(state, |child_state, _| {
                next_states.insert(child_state);
            });
        }
    }

    states
}

fn with_partial_state_children(
    state: &PartialSkillCheckState,
    mut consumer: impl FnMut(PartialSkillCheckState, Probability),
) {
    if state.inaptitude && state.fixed_rolls.iter().all(Option::is_some) {
        let mut child_state = state.clone();
        child_state.apply_inaptitude();
        consumer(child_state, Probability::ONE);
        return;
    }

    let first_unrolled_idx = state.fixed_rolls.iter().position(Option::is_none).unwrap();

    let rolls_to_analyze = if let Some(cap) = state.roll_caps[first_unrolled_idx] {
        // evaluate rolling >= the cap first, then the rest in the loop below
        let roll_probability = cap_probability(cap);
        let mut child_state = state.clone();
        child_state.fixed_rolls[first_unrolled_idx] = Some(cap);
        child_state.roll_caps[first_unrolled_idx] = None;
        consumer(child_state, roll_probability);

        Roll::ALL.split(|&roll| roll == cap).next().unwrap()
    }
    else {
        // no cap given, all options are equally likely
        &Roll::ALL
    };

    for &roll in rolls_to_analyze {
        let mut child_state = state.clone();
        child_state.fixed_rolls[first_unrolled_idx] = Some(roll);
        child_state.roll_caps[first_unrolled_idx] = None;
        consumer(child_state, DIE_RESULT_PROBABILITY);
    }
}
