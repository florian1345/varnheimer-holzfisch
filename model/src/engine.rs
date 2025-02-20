use crate::check::{
    PartialSkillCheckState,
    SkillCheckAction,
    SkillCheckProbabilities,
    SkillCheckState
};
use crate::evaluation::Evaluated;

pub trait SkillCheckEngine {

    fn evaluate_action(&mut self, state: SkillCheckState, action: SkillCheckAction)
        -> Evaluated<SkillCheckProbabilities>;

    fn evaluate_partial(&mut self, state: PartialSkillCheckState)
        -> Evaluated<SkillCheckProbabilities>;

    fn evaluate(&mut self, skill_check: SkillCheckState)
            -> Vec<(SkillCheckAction, Evaluated<SkillCheckProbabilities>)> {
        let mut result = skill_check.legal_actions().into_iter()
            .map(|action| (action, self.evaluate_action(skill_check, action)))
            .collect::<Vec<_>>();

        result.sort_by_key(|(_, evaluated)| evaluated.evaluation);

        result
    }
}
