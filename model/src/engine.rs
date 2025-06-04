use crate::check::outcome::SkillCheckOutcomeProbabilities;
use crate::check::{PartialSkillCheckState, SkillCheckAction, SkillCheckState};
use crate::evaluation::Evaluated;

pub trait SkillCheckEngine {
    fn evaluate_action(
        &mut self,
        state: SkillCheckState,
        action: SkillCheckAction,
    ) -> Evaluated<SkillCheckOutcomeProbabilities>;

    fn evaluate_partial(
        &mut self,
        state: PartialSkillCheckState,
    ) -> Evaluated<SkillCheckOutcomeProbabilities>;

    fn evaluate_all_actions(
        &mut self,
        skill_check: SkillCheckState,
    ) -> Vec<(SkillCheckAction, Evaluated<SkillCheckOutcomeProbabilities>)> {
        let mut result = skill_check
            .legal_actions()
            .into_iter()
            .map(|action| (action, self.evaluate_action(skill_check.clone(), action)))
            .collect::<Vec<_>>();

        result.sort_by_key(|(_, evaluated)| -evaluated.evaluation);

        result
    }

    fn evaluate(
        &mut self,
        skill_check: SkillCheckState,
    ) -> Evaluated<SkillCheckOutcomeProbabilities> {
        self.evaluate_all_actions(skill_check)
            .into_iter()
            .map(|(_, evaluated)| evaluated)
            .max_by_key(|evaluated| evaluated.evaluation)
            .unwrap()
    }
}
