use std::collections::HashMap;

use rayon::prelude::*;

use crate::check::outcome::{
    SkillCheckOutcome,
    SkillCheckOutcomeKind,
    SkillCheckOutcomeProbabilities,
};
use crate::check::{
    DICE_PER_SKILL_CHECK,
    PartialSkillCheckState,
    SkillCheckAction,
    SkillCheckActionResult,
    SkillCheckState,
};
use crate::evaluation::{Evaluated, Evaluation, SkillCheckEvaluator};
use crate::probability::Probability;
use crate::roll::{DICE_SIDES, Roll};
use crate::skill::{Attribute, SkillPoints};

trait HasSkill {
    fn skill_value_mut(&mut self) -> &mut SkillPoints;

    fn attributes_mut(&mut self) -> &mut [Attribute];
}

impl HasSkill for SkillCheckState {
    fn skill_value_mut(&mut self) -> &mut SkillPoints {
        &mut self.skill_value
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        &mut self.attributes
    }
}

impl HasSkill for PartialSkillCheckState {
    fn skill_value_mut(&mut self) -> &mut SkillPoints {
        &mut self.skill_value
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        &mut self.attributes
    }
}

fn normalize(state: &mut impl HasSkill) {
    let roll_1 = Roll::new(1).unwrap();
    let attribute_1 = Attribute::new(1);
    let attribute_20 = Attribute::new(20);
    let mut missing_skill_points = SkillPoints::new(0);

    for attribute in state.attributes_mut() {
        missing_skill_points += attribute.missing_skill_points(roll_1);
        *attribute = (*attribute).max(attribute_1).min(attribute_20);
    }

    *state.skill_value_mut() -= missing_skill_points;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct PartialOutcome {
    min_rolls: usize,
    max_rolls: usize,
    skill_points: SkillPoints,
}

impl PartialOutcome {
    fn new(skill_value: SkillPoints) -> Self {
        Self {
            min_rolls: 0,
            max_rolls: 0,
            skill_points: skill_value,
        }
    }

    fn apply_roll(mut self, roll: Roll, attribute: Attribute) -> Self {
        if roll == Roll::MAX {
            self.max_rolls += 1;

            if self.max_rolls >= DICE_PER_SKILL_CHECK - 1 {
                // Make sure skill points of all failures are equal to maximize overlap
                self.skill_points = SkillPoints::new(-1);
                return self;
            }
        }
        else if roll == Roll::MIN {
            self.min_rolls += 1;
        }

        self.skill_points -= attribute.missing_skill_points(roll);

        if self.min_rolls >= DICE_PER_SKILL_CHECK - 1 {
            // Success by definition => at least QL 1
            // Set skill points to 3 to maximize overlap
            self.skill_points = self.skill_points.max(SkillPoints::new(3));
        }
        else {
            // Make sure skill points of all failures are equal to maximize overlap
            self.skill_points = self.skill_points.max(SkillPoints::new(-1));
        }

        self
    }

    fn apply_known_rolls(mut self, state: &PartialSkillCheckState) -> Self {
        let known_rolls = state
            .fixed_rolls
            .into_iter()
            .zip(state.attributes)
            .filter_map(|(roll, attribute)| roll.map(|roll| (roll, attribute)));

        for (roll, attribute) in known_rolls {
            self = self.apply_roll(roll, attribute);
        }

        self
    }

    fn to_outcome_kind(self) -> SkillCheckOutcomeKind {
        if self.min_rolls < DICE_PER_SKILL_CHECK - 1 && self.max_rolls < DICE_PER_SKILL_CHECK - 1 {
            self.skill_points
                .quality_level()
                .map(SkillCheckOutcomeKind::Success)
                .unwrap_or(SkillCheckOutcomeKind::Failure)
        }
        else if self.min_rolls == DICE_PER_SKILL_CHECK - 1 {
            SkillCheckOutcomeKind::CriticalSuccess(self.skill_points.quality_level().unwrap())
        }
        else if self.max_rolls == DICE_PER_SKILL_CHECK - 1 {
            SkillCheckOutcomeKind::CriticalFailure
        }
        else if self.min_rolls == DICE_PER_SKILL_CHECK {
            SkillCheckOutcomeKind::SpectacularSuccess(self.skill_points.quality_level().unwrap())
        }
        else {
            SkillCheckOutcomeKind::SpectacularFailure
        }
    }

    fn to_outcome_without_modifiers(self) -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: self.to_outcome_kind(),
            remaining_modifiers: Default::default(),
        }
    }
}

struct PartialOutcomeProbabilities(HashMap<PartialOutcome, Probability>);

impl PartialOutcomeProbabilities {
    fn new() -> PartialOutcomeProbabilities {
        PartialOutcomeProbabilities(HashMap::new())
    }

    fn of_known_outcome(outcome: PartialOutcome) -> PartialOutcomeProbabilities {
        PartialOutcomeProbabilities(HashMap::from([(outcome, Probability::ONE)]))
    }

    fn add(&mut self, outcome: PartialOutcome, probability: Probability) {
        *self.0.entry(outcome).or_insert(Probability::ZERO) += probability;
    }

    fn into_skill_check_outcome_probabilities_without_modifiers(
        self,
    ) -> SkillCheckOutcomeProbabilities {
        let mut outcome_probabilities = SkillCheckOutcomeProbabilities::default();

        for (outcome, probability) in self.0.into_iter() {
            outcome_probabilities.add_outcome(outcome.to_outcome_without_modifiers(), probability);
        }

        outcome_probabilities
    }
}

fn cap_probability(cap: Roll) -> Probability {
    let rolls_at_least_cap = (Roll::MAX.as_u8() - cap.as_u8()) as usize + 1;
    DIE_RESULT_PROBABILITY.saturating_mul(rolls_at_least_cap)
}

#[derive(Clone)]
pub struct VarnheimerHolzfischEngine<EvaluatorT> {
    pub evaluator: EvaluatorT,
}

const DIE_RESULT_PROBABILITY: Probability = Probability::new(1.0f64 / DICE_SIDES as f64).unwrap();

impl<EvaluatorT> VarnheimerHolzfischEngine<EvaluatorT>
where
    EvaluatorT: SkillCheckEvaluator + Send + Sync,
    EvaluatorT::Error: Send,
{
    fn evaluate_without_modifiers(
        &self,
        state: PartialSkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        let outcome = PartialOutcome::new(state.skill_value).apply_known_rolls(&state);
        let unknown_rolls = state
            .fixed_rolls
            .into_iter()
            .zip(state.attributes)
            .zip(state.roll_caps)
            .filter(|((roll, _), _)| roll.is_none())
            .map(|((_, attribute), cap)| (attribute, cap));
        let mut current_outcome_probabilities =
            PartialOutcomeProbabilities::of_known_outcome(outcome);

        for (attribute, cap) in unknown_rolls {
            let mut next_outcome_probabilities = PartialOutcomeProbabilities::new();
            let mut process_roll = |roll, roll_probability| {
                for (outcome, &outcome_probability) in current_outcome_probabilities.0.iter() {
                    next_outcome_probabilities.add(
                        outcome.apply_roll(roll, attribute),
                        roll_probability * outcome_probability,
                    );
                }
            };

            let rolls_to_analyze = if let Some(cap) = cap {
                process_roll(cap, cap_probability(cap));

                Roll::ALL.split(|&roll| roll == cap).next().unwrap()
            }
            else {
                &Roll::ALL
            };

            for &roll in rolls_to_analyze {
                process_roll(roll, DIE_RESULT_PROBABILITY);
            }

            current_outcome_probabilities = next_outcome_probabilities;
        }

        let outcome_probabilities = current_outcome_probabilities
            .into_skill_check_outcome_probabilities_without_modifiers();
        let evaluation = self
            .evaluator
            .evaluate_probabilities(&outcome_probabilities)?;

        Ok(Evaluated {
            evaluated: outcome_probabilities,
            evaluation,
        })
    }

    pub fn evaluate_action(
        &self,
        state: SkillCheckState,
        action: SkillCheckAction,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        match action.apply(state) {
            SkillCheckActionResult::Done(outcome) => {
                let evaluation = self.evaluator.evaluate(&outcome)?;

                Ok(Evaluated {
                    evaluated: SkillCheckOutcomeProbabilities::of_known_outcome(outcome),
                    evaluation,
                })
            },
            SkillCheckActionResult::State(state) => self.evaluate_rec(state),
            SkillCheckActionResult::PartialState(partial_state) => {
                self.evaluate_partial_rec(partial_state)
            },
        }
    }

    fn evaluate_partial_rec(
        &self,
        mut state: PartialSkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        if let Some(state) = state.as_skill_check_state() {
            return self.evaluate_rec(state);
        }

        if state.inaptitude && state.fixed_rolls.iter().all(Option::is_some) {
            state.apply_inaptitude();
        }

        if state.modifiers.is_empty() && !state.inaptitude {
            return self.evaluate_without_modifiers(state);
        }

        let first_unrolled_idx = state.fixed_rolls.iter().position(Option::is_none).unwrap();
        let mut probabilities = SkillCheckOutcomeProbabilities::default();
        let mut evaluation = Evaluation::ZERO;

        let rolls_to_analyze = if let Some(cap) = state.roll_caps[first_unrolled_idx] {
            // evaluate rolling >= the cap first, then the rest in the loop below
            let probability = cap_probability(cap);
            let mut child_state = state.clone();
            child_state.fixed_rolls[first_unrolled_idx] = Some(cap);
            let sub_evaluated = self.evaluate_partial_rec(child_state)?;
            probabilities.saturating_add_assign(&(sub_evaluated.evaluated.clone() * probability));
            evaluation += sub_evaluated.evaluation * probability;

            Roll::ALL.split(|&roll| roll == cap).next().unwrap()
        }
        else {
            // no cap given, all options are equally likely
            &Roll::ALL
        };

        for &roll in rolls_to_analyze {
            let mut child_state = state.clone();
            child_state.fixed_rolls[first_unrolled_idx] = Some(roll);
            let sub_evaluated = self.evaluate_partial_rec(child_state)?;
            probabilities
                .saturating_add_assign(&(sub_evaluated.evaluated.clone() * DIE_RESULT_PROBABILITY));
            evaluation += sub_evaluated.evaluation * DIE_RESULT_PROBABILITY;
        }

        Ok(Evaluated {
            evaluated: probabilities,
            evaluation,
        })
    }

    pub fn evaluate_partial(
        &self,
        mut state: PartialSkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        normalize(&mut state);
        self.evaluate_partial_rec(state)
    }

    pub fn evaluate_all_actions(
        &self,
        skill_check: SkillCheckState,
    ) -> Result<Vec<(SkillCheckAction, Evaluated<SkillCheckOutcomeProbabilities>)>, EvaluatorT::Error>
    {
        let mut result = skill_check
            .legal_actions()
            .into_par_iter()
            .map(|action| Ok((action, self.evaluate_action(skill_check.clone(), action)?)))
            .collect::<Result<Vec<_>, _>>()?;

        result.sort_by_key(|(_, evaluated)| -evaluated.evaluation);

        Ok(result)
    }

    fn evaluate_rec(
        &self,
        skill_check: SkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        Ok(self
            .evaluate_all_actions(skill_check)?
            .into_iter()
            .map(|(_, evaluated)| evaluated)
            .max_by_key(|evaluated| evaluated.evaluation)
            .unwrap())
    }

    pub fn evaluate(
        &self,
        mut skill_check: SkillCheckState,
    ) -> Result<Evaluated<SkillCheckOutcomeProbabilities>, EvaluatorT::Error> {
        normalize(&mut skill_check);
        self.evaluate_rec(skill_check)
    }
}
