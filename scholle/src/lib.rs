use model::check::outcome::SkillCheckOutcome;
use model::evaluation::{Evaluation, SkillCheckEvaluator};

use crate::context::Expression;
use crate::error::{InitializationResult, RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::runtime::{Value, evaluate};

pub(crate) mod builtin;
pub mod context;
pub mod error;
pub mod lexer;
pub mod operators;
pub mod parser;
pub mod runtime;
pub mod span;

#[derive(Clone)]
pub struct ScholleEvaluator {
    expression: Expression,
}

impl ScholleEvaluator {
    pub fn new(code: impl AsRef<str>) -> InitializationResult<ScholleEvaluator> {
        let tokens = lexer::lex(code.as_ref())?;
        let expression_node = parser::parse(tokens)?;
        let expression = context::analyze(expression_node)?;

        Ok(ScholleEvaluator { expression })
    }
}

impl SkillCheckEvaluator for ScholleEvaluator {
    type Error = RuntimeError;

    fn evaluate(&self, outcome: &SkillCheckOutcome) -> RuntimeResult<Evaluation> {
        let value = evaluate(&self.expression, outcome)?;

        match value {
            Value::Integer(value) => Ok(Evaluation::new(value as f64).unwrap()),
            Value::Float(value) => Evaluation::new(value).ok_or(RuntimeError {
                kind: RuntimeErrorKind::InvalidResult(value),
                span: self.expression.span,
            }),
            _ => panic!("Scholle expression did not evaluate to number"),
        }
    }
}

#[cfg(test)]
mod tests {
    use kernal::prelude::*;
    use model::check::outcome::SkillCheckOutcomeKind;
    use model::skill::QualityLevel;
    use rstest::rstest;

    use super::*;

    fn outcome() -> SkillCheckOutcome {
        SkillCheckOutcome {
            kind: SkillCheckOutcomeKind::Success(QualityLevel::TWO),
            remaining_modifiers: Default::default(),
        }
    }

    #[rstest]
    #[case("quality_level", 2.0)]
    #[case("as_float(quality_level) / 4.0", 0.5)]
    fn valid(#[case] code: &str, #[case] expected_value: f64) {
        let evaluator = ScholleEvaluator::new(code).unwrap();

        let evaluation = evaluator.evaluate(&outcome());

        assert_that!(evaluation).contains_value(Evaluation::new(expected_value).unwrap());
    }

    #[rstest]
    #[case("0.0 / 0.0", f64::NAN)]
    #[case("pow(10.0, 1000.0)", f64::INFINITY)]
    #[case("-pow(10.0, 1000.0)", f64::NEG_INFINITY)]
    fn invalid_value(#[case] code: &str, #[case] expected_value: f64) {
        let evaluator = ScholleEvaluator::new(code).unwrap();

        let evaluation = evaluator.evaluate(&outcome());

        assert_that!(evaluation).contains_error(RuntimeError {
            kind: RuntimeErrorKind::InvalidResult(expected_value),
            span: (0..code.len()).into(),
        });
    }
}
