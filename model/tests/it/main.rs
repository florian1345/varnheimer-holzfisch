use model::check::modifier::ModifierState;
use model::check::outcome::{SkillCheckOutcome, SkillCheckOutcomeKind};
use model::engine::VarnheimerHolzfischEngine;
use model::evaluation::Evaluation;
use model::probability::Probability;
use model::roll::Roll;
use model::skill::QualityLevel;
use scholle::ScholleEvaluator;

mod evaluate_partial;
mod skill_checks;

pub fn engine(scholle_code: impl AsRef<str>) -> VarnheimerHolzfischEngine<ScholleEvaluator> {
    let evaluator = ScholleEvaluator::new(scholle_code).unwrap();
    VarnheimerHolzfischEngine { evaluator }
}

pub fn prob(value: f64) -> Probability {
    Probability::new(value).unwrap()
}

pub fn eval(value: f64) -> Evaluation {
    Evaluation::new(value).unwrap()
}

pub fn outcome_no_fate_points(kind: SkillCheckOutcomeKind) -> SkillCheckOutcome {
    SkillCheckOutcome {
        kind,
        remaining_modifiers: ModifierState::default(),
    }
}

pub fn outcome_prob(
    outcome: SkillCheckOutcome,
    probability: f64,
) -> (SkillCheckOutcome, Probability) {
    (outcome, prob(probability))
}

pub fn outcome_prob_no_mod(
    kind: SkillCheckOutcomeKind,
    probability: f64,
) -> (SkillCheckOutcome, Probability) {
    outcome_prob(outcome_no_fate_points(kind), probability)
}

pub const EPS: f64 = 1e-6;

pub const QL_1: QualityLevel = QualityLevel::ONE;
pub const QL_2: QualityLevel = QualityLevel::TWO;
pub const QL_3: QualityLevel = QualityLevel::THREE;
pub const QL_4: QualityLevel = QualityLevel::FOUR;
pub const QL_5: QualityLevel = QualityLevel::FIVE;
pub const QL_6: QualityLevel = QualityLevel::SIX;

fn roll(roll: u8) -> Roll {
    Roll::new(roll).unwrap()
}
