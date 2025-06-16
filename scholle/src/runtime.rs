use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use std::ops::{Add, Deref, Div, Mul, Rem, Sub};
use std::panic::RefUnwindSafe;
use std::rc::Rc;
use std::sync::Arc;

use model::check::outcome::SkillCheckOutcome;

use crate::builtin;
use crate::context::{DeclarationId, Expression, ExpressionKind};
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::operators::{BinaryOperator, UnaryOperator};

pub type BuiltinFunctionImpl = dyn Fn(Vec<Value>) -> Value + RefUnwindSafe;

#[derive(Clone)]
pub struct BuiltinFunction {
    implementation: Arc<BuiltinFunctionImpl>,
}

impl BuiltinFunction {
    pub(crate) fn new(
        implementation: impl Fn(Vec<Value>) -> Value + RefUnwindSafe + 'static,
    ) -> BuiltinFunction {
        BuiltinFunction {
            implementation: Arc::new(implementation),
        }
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "builtin function @ {:x}",
            Arc::as_ptr(&self.implementation).addr()
        )
    }
}

impl Deref for BuiltinFunction {
    type Target = BuiltinFunctionImpl;

    fn deref(&self) -> &BuiltinFunctionImpl {
        self.implementation.deref()
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Function {
        captured_environment: Rc<StackFrame>,
        parameter_ids: Rc<[DeclarationId]>,
        body: Rc<Expression>,
    },
    BuiltinFunction(BuiltinFunction),
}

#[derive(Clone, Debug)]
pub struct StackFrame {
    values: HashMap<DeclarationId, Value>,
}

impl StackFrame {
    fn new() -> StackFrame {
        StackFrame {
            values: HashMap::new(),
        }
    }

    fn get(&self, declaration_id: DeclarationId) -> Value {
        self.values
            .get(&declaration_id)
            .cloned()
            .expect("stack frame does not contain referenced value")
    }

    fn enter(&mut self, declaration_id: DeclarationId, value: Value) {
        self.values.insert(declaration_id, value);
    }
}

struct EvaluationContext {
    stack: Vec<StackFrame>,
}

impl EvaluationContext {
    fn new(outcome: &SkillCheckOutcome) -> EvaluationContext {
        let mut root_frame = StackFrame::new();

        for builtin in builtin::runtime_builtins(outcome) {
            root_frame.enter(builtin.declaration_id, builtin.value);
        }

        EvaluationContext {
            stack: vec![root_frame],
        }
    }

    fn current_frame(&self) -> &StackFrame {
        self.stack.last().expect("empty evaluation stack")
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().expect("empty evaluation stack")
    }

    fn push(&mut self, captured_environment: StackFrame) {
        self.stack.push(captured_environment);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }
}

fn evaluate_arithmetic(
    expression: &Expression,
    lhs: Value,
    rhs: Value,
    int_op: impl FnOnce(i64, i64) -> Option<i64>,
    float_op: impl FnOnce(f64, f64) -> f64,
    error_kind_on_failure: RuntimeErrorKind,
) -> RuntimeResult<Value> {
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => int_op(lhs, rhs)
            .ok_or(RuntimeError {
                kind: error_kind_on_failure,
                span: expression.span,
            })
            .map(Value::Integer),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(float_op(lhs, rhs))),
        (lhs, rhs) => {
            panic!(
                "operands {:?} and {:?} have invalid types for arithmetic operation",
                lhs, rhs
            )
        },
    }
}

fn evaluate_comparison(
    lhs: Value,
    rhs: Value,
    int_op: impl FnOnce(&i64, &i64) -> bool,
    float_op: impl FnOnce(&f64, &f64) -> bool,
) -> Value {
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Bool(int_op(&lhs, &rhs)),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Bool(float_op(&lhs, &rhs)),
        (lhs, rhs) => {
            panic!(
                "operands {:?} and {:?} have invalid types for comparison operation",
                lhs, rhs
            )
        },
    }
}

fn evaluate_equality(
    lhs: Value,
    rhs: Value,
    int_op: impl FnOnce(&i64, &i64) -> bool,
    float_op: impl FnOnce(&f64, &f64) -> bool,
    bool_op: impl FnOnce(&bool, &bool) -> bool,
) -> Value {
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Bool(int_op(&lhs, &rhs)),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Bool(float_op(&lhs, &rhs)),
        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(bool_op(&lhs, &rhs)),
        (lhs, rhs) => {
            panic!(
                "operands {:?} and {:?} have invalid types for equality operation",
                lhs, rhs
            )
        },
    }
}

fn evaluate_boolean(lhs: Value, rhs: Value, op: impl FnOnce(bool, bool) -> bool) -> Value {
    if let (&Value::Bool(lhs), &Value::Bool(rhs)) = (&lhs, &rhs) {
        Value::Bool(op(lhs, rhs))
    }
    else {
        panic!(
            "operands {:?} and {:?} have invalid types for boolean operation",
            lhs, rhs
        )
    }
}

fn evaluate_ctx(expression: &Expression, ctx: &mut EvaluationContext) -> RuntimeResult<Value> {
    let value = match &expression.kind {
        ExpressionKind::IntLiteral(value) => Value::Integer(*value),
        ExpressionKind::FloatLiteral(value) => Value::Float(*value),
        ExpressionKind::BoolLiteral(value) => Value::Bool(*value),
        ExpressionKind::Reference(id) => ctx.current_frame().get(*id),
        ExpressionKind::UnaryOperation { operand, operator } => {
            let operand = evaluate_ctx(operand, ctx)?;

            match (operand, operator) {
                (Value::Bool(value), UnaryOperator::Not) => Value::Bool(!value),
                (Value::Integer(value), UnaryOperator::Negative) => {
                    Value::Integer(value.checked_neg().ok_or(RuntimeError {
                        kind: RuntimeErrorKind::ArithmeticOverflow,
                        span: expression.span,
                    })?)
                },
                (Value::Float(value), UnaryOperator::Negative) => Value::Float(-value),
                operand => {
                    panic!(
                        "value {:?} cannot be applied to unary operator {:?}",
                        operand, operator
                    )
                },
            }
        },
        ExpressionKind::BinaryOperation { lhs, rhs, operator } => {
            let lhs = evaluate_ctx(lhs, ctx)?;
            let rhs = evaluate_ctx(rhs, ctx)?;

            match operator {
                BinaryOperator::Addition => evaluate_arithmetic(
                    expression,
                    lhs,
                    rhs,
                    i64::checked_add,
                    f64::add,
                    RuntimeErrorKind::ArithmeticOverflow,
                )?,
                BinaryOperator::Subtraction => evaluate_arithmetic(
                    expression,
                    lhs,
                    rhs,
                    i64::checked_sub,
                    f64::sub,
                    RuntimeErrorKind::ArithmeticOverflow,
                )?,
                BinaryOperator::Multiplication => evaluate_arithmetic(
                    expression,
                    lhs,
                    rhs,
                    i64::checked_mul,
                    f64::mul,
                    RuntimeErrorKind::ArithmeticOverflow,
                )?,
                BinaryOperator::Division => evaluate_arithmetic(
                    expression,
                    lhs,
                    rhs,
                    i64::checked_div,
                    f64::div,
                    RuntimeErrorKind::DivideByZero,
                )?,
                BinaryOperator::Modulo => evaluate_arithmetic(
                    expression,
                    lhs,
                    rhs,
                    i64::checked_rem,
                    f64::rem,
                    RuntimeErrorKind::DivideByZero,
                )?,
                BinaryOperator::Less => evaluate_comparison(lhs, rhs, i64::lt, f64::lt),
                BinaryOperator::Greater => evaluate_comparison(lhs, rhs, i64::gt, f64::gt),
                BinaryOperator::LessEqual => evaluate_comparison(lhs, rhs, i64::le, f64::le),
                BinaryOperator::GreaterEqual => evaluate_comparison(lhs, rhs, i64::ge, f64::ge),
                BinaryOperator::Equal => evaluate_equality(lhs, rhs, i64::eq, f64::eq, bool::eq),
                BinaryOperator::NotEqual => evaluate_equality(lhs, rhs, i64::ne, f64::ne, bool::ne),
                BinaryOperator::And => evaluate_boolean(lhs, rhs, |lhs, rhs| lhs && rhs),
                BinaryOperator::Or => evaluate_boolean(lhs, rhs, |lhs, rhs| lhs || rhs),
                BinaryOperator::Xor => evaluate_boolean(lhs, rhs, |lhs, rhs| lhs ^ rhs),
            }
        },
        ExpressionKind::Call { callee, arguments } => {
            let callee = evaluate_ctx(callee, ctx)?;
            let arguments = arguments
                .iter()
                .map(|argument| evaluate_ctx(argument, ctx))
                .collect::<RuntimeResult<Vec<_>>>()?;

            match callee {
                Value::Function {
                    captured_environment,
                    parameter_ids,
                    body,
                } => {
                    assert_eq!(
                        arguments.len(),
                        parameter_ids.len(),
                        "incorrect number of arguments"
                    );

                    ctx.push(captured_environment.as_ref().clone());

                    arguments.into_iter().zip(parameter_ids.as_ref()).for_each(
                        |(value, &parameter_id)| ctx.current_frame_mut().enter(parameter_id, value),
                    );

                    let result = evaluate_ctx(&body, ctx)?;

                    ctx.pop();

                    result
                },
                Value::BuiltinFunction(builtin_function) => builtin_function(arguments),
                callee => panic!("value {:?} is not a callable function", callee),
            }
        },
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let Value::Bool(condition) = evaluate_ctx(condition, ctx)?
            else {
                panic!("condition of if-expression is not a boolean")
            };

            evaluate_ctx(if condition { then_branch } else { else_branch }, ctx)?
        },
        ExpressionKind::Let { assignments, body } => {
            // Note: Scopes do not exist at runtime. Since declarations have unique IDs, we can
            // simply add all assigned values to the environment and not remove them after.

            for assignment in assignments {
                let value = evaluate_ctx(&assignment.value, ctx)?;
                ctx.current_frame_mut().enter(assignment.id, value);
            }

            evaluate_ctx(body, ctx)?
        },
        ExpressionKind::Lambda { parameters, body } => Value::Function {
            captured_environment: Rc::new(ctx.current_frame().clone()),
            parameter_ids: parameters.iter().map(|parameter| parameter.id).collect(),
            body: Rc::new((**body).clone()),
        },
    };

    Ok(value)
}

/// Evaluates the given [Expression] with values for the builtins (such as `quality_level`) derived
/// from the given [SkillCheckOutcome]. The expression is expected to have gone through ordinary
/// contextual analysis. Any errors that come up during the evaluation which should have been caught
/// by the contextual analysis will cause this function to panic. All other errors are communicated
/// as [RuntimeError]s.
pub fn evaluate(expression: &Expression, outcome: &SkillCheckOutcome) -> RuntimeResult<Value> {
    let mut ctx = EvaluationContext::new(outcome);
    evaluate_ctx(expression, &mut ctx)
}

#[cfg(test)]
mod tests {
    use kernal::prelude::*;
    use model::check::modifier::{Aptitude, Modifier, ModifierState};
    use model::check::outcome::SkillCheckOutcomeKind;
    use model::check::outcome::SkillCheckOutcomeKind::*;
    use model::skill::{QualityLevel, SkillPoints};
    use rstest::rstest;

    use super::*;
    use crate::context::analyze;
    use crate::span::CodeSpan;
    use crate::{lexer, parser};

    #[derive(Debug)]
    enum ExpectedValue {
        Integer(i64),
        Float(f64),
    }

    impl From<i64> for ExpectedValue {
        fn from(value: i64) -> ExpectedValue {
            ExpectedValue::Integer(value)
        }
    }

    impl From<f64> for ExpectedValue {
        fn from(value: f64) -> ExpectedValue {
            ExpectedValue::Float(value)
        }
    }

    fn parse_and_analyze(code: &str) -> Expression {
        let tokens = lexer::lex(code).unwrap();
        let ast = parser::parse(tokens).unwrap();
        analyze(ast).unwrap()
    }

    trait WithModifiers {
        fn with_modifiers(self, modifiers: impl IntoIterator<Item = Modifier>)
        -> SkillCheckOutcome;

        fn without_modifiers(self) -> SkillCheckOutcome;
    }

    impl WithModifiers for SkillCheckOutcomeKind {
        fn with_modifiers(
            self,
            modifiers: impl IntoIterator<Item = Modifier>,
        ) -> SkillCheckOutcome {
            SkillCheckOutcome {
                kind: self,
                remaining_modifiers: ModifierState::from_modifiers(modifiers),
            }
        }

        fn without_modifiers(self) -> SkillCheckOutcome {
            SkillCheckOutcome {
                kind: self,
                remaining_modifiers: ModifierState::default(),
            }
        }
    }

    fn aptitude(max_dice: usize) -> Modifier {
        Modifier::Aptitude(Aptitude::new(max_dice).unwrap())
    }

    #[rstest]
    #[case::int_literal("123", Failure.without_modifiers(), 123)]
    #[case::float_literal("123.456", Failure.without_modifiers(), 123.456)]
    #[case::arithmetic_1(
        "quality_level + 2 * remaining_fate_points",
        Success(QualityLevel::THREE).with_modifiers([Modifier::FatePoint]),
        5,
    )]
    #[case::arithmetic_2(
        "as_float(quality_level) / as_float(2 - remaining_fate_points)",
        Success(QualityLevel::FIVE).with_modifiers([aptitude(1)]),
        2.5,
    )]
    #[case::arithmetic_3("-2 % 3", Failure.without_modifiers(), -2)]
    #[case::simple_if_1("if is_success then 1 else 0", Success(QualityLevel::ONE).without_modifiers(), 1)]
    #[case::simple_if_2("if is_success then 1 else 0", Failure.without_modifiers(), 0)]
    #[case::and_not_1(
        "if is_critical_success & !is_spectacular_success then quality_level else 0",
        Success(QualityLevel::FOUR).without_modifiers(),
        0,
    )]
    #[case::and_not_2(
        "if is_critical_success & !is_spectacular_success then quality_level else 0",
        CriticalSuccess(QualityLevel::FOUR).without_modifiers(),
        4,
    )]
    #[case::and_not_3(
        "if is_critical_success & !is_spectacular_success then quality_level else 0",
        SpectacularSuccess(QualityLevel::FOUR).without_modifiers(),
        0,
    )]
    #[case::or_1(
        "(if is_critical_failure | is_critical_success then 2 else 1) * (quality_level - 1)",
        Failure.without_modifiers(),
        -1,
    )]
    #[case::or_2(
        "(if is_critical_failure | is_critical_success then 2 else 1) * (quality_level - 1)",
        CriticalFailure.without_modifiers(),
        -2,
    )]
    #[case::or_3(
        "(if is_critical_failure | is_critical_success then 2 else 1) * (quality_level - 1)",
        Success(QualityLevel::SIX).without_modifiers(),
        5,
    )]
    #[case::or_4(
        "(if is_critical_failure | is_critical_success then 2 else 1) * (quality_level - 1)",
        SpectacularSuccess(QualityLevel::SIX).without_modifiers(),
        10,
    )]
    #[case::xor_1("if true ^ true then 1 else 0", Failure.without_modifiers(), 0)]
    #[case::xor_2("if true ^ false then 1 else 0", Failure.without_modifiers(), 1)]
    #[case::xor_3("if false ^ true then 1 else 0", Failure.without_modifiers(), 1)]
    #[case::xor_4("if false ^ false then 1 else 0", Failure.without_modifiers(), 0)]
    #[case::comparison_1(
        "if quality_level > 2 then 1 else 0",
        Success(QualityLevel::TWO).without_modifiers(),
        0,
    )]
    #[case::comparison_2(
        "if quality_level > 2 then 1 else 0",
        Success(QualityLevel::THREE).without_modifiers(),
        1,
    )]
    #[case::comparison_3(
        "if quality_level >= 2 then 1 else 0",
        Success(QualityLevel::ONE).without_modifiers(),
        0,
    )]
    #[case::comparison_4(
        "if quality_level >= 2 then 1 else 0",
        Success(QualityLevel::TWO).without_modifiers(),
        1,
    )]
    #[case::comparison_5(
        "if as_float(remaining_aptitudes(1)) < 1.0 then quality_level else quality_level + 1",
        Success(QualityLevel::FOUR).with_modifiers([aptitude(1)]),
        5,
    )]
    #[case::comparison_6(
        "if as_float(remaining_aptitudes(1)) < 1.0 then quality_level else quality_level + 1",
        Success(QualityLevel::FOUR).with_modifiers([aptitude(2)]),
        4,
    )]
    #[case::comparison_7(
        "if as_float(remaining_extra_quality_levels_on_success) <= 1.0 then 0.0 else 0.1",
        Failure.with_modifiers([
            Modifier::ExtraQualityLevelOnSuccess,
            Modifier::ExtraQualityLevelOnSuccess
        ]),
        0.1,
    )]
    #[case::comparison_8(
        "if as_float(remaining_extra_quality_levels_on_success) <= 1.0 then 0.0 else 0.1",
        Failure.with_modifiers([Modifier::ExtraQualityLevelOnSuccess]),
        0.0,
    )]
    #[case::equality_1(
        "if is_critical_success != true then 1 else 0",
        Failure.without_modifiers(),
        1,
    )]
    #[case::equality_2(
        "if quality_level != 3 then 1 else 0",
        Success(QualityLevel::THREE).without_modifiers(),
        0,
    )]
    #[case::equality_3("if pow(2.0, 0.5) != 1.4 then 1 else 0", Failure.without_modifiers(), 1)]
    #[case::equality_4(
        "if is_critical_success == true then 1 else 0",
        Failure.without_modifiers(),
        0,
    )]
    #[case::equality_5(
        "if quality_level == 3 then 1 else 0",
        Success(QualityLevel::THREE).without_modifiers(),
        1,
    )]
    #[case::equality_6("if pow(2.0, 0.5) == 1.4 then 1 else 0", Failure.without_modifiers(), 0)]
    #[case::let_1(
        "let x = quality_level + 1 in x * x",
        Success(QualityLevel::TWO).without_modifiers(),
        9,
    )]
    #[case::let_2("let x = 1.5, y = x / 2.0 in y / 2.0", Failure.without_modifiers(), 0.375)]
    #[case::let_3("let x = 2, x = x + 1, x = x + 1 in x", Failure.without_modifiers(), 4)]
    #[case::let_4(
        "let x = (let y = 3 in y * y) in (let y = x * x in y)",
        Failure.without_modifiers(),
        81,
    )]
    #[case::let_reassign_builtin_function(
        "let f = remaining_extra_skill_points_on_success in f(3)",
        Failure.without_modifiers(),
        0,
    )]
    #[case::lambda_no_params(
        "let f = () -> quality_level in f()",
        Success(QualityLevel::THREE).without_modifiers(),
        3,
    )]
    #[case::lambda_simple_param(
        "let f = (x: int) -> x * x in f(remaining_extra_skill_points(2))",
        Failure.with_modifiers([
            Modifier::ExtraSkillPoints(SkillPoints::new(1)),
            Modifier::ExtraSkillPoints(SkillPoints::new(2)),
            Modifier::ExtraSkillPoints(SkillPoints::new(2)),
        ]),
        4,
    )]
    #[case::lambda_multiple_params(
        "let \
            f = (x: int, y: float, z: bool) -> if z then pow(as_float(x), y) else 0.0 \
        in \
            f(quality_level, 2.5, !is_spectacular_failure)",
        CriticalSuccess(QualityLevel::FOUR).without_modifiers(),
        32.0,
    )]
    #[case::lambda_with_environment_1(
        "let x = 3, f = (delta: int) -> x + delta in let x = 4 in f(-1) * f(0) + x",
        Failure.without_modifiers(),
        10,
    )]
    #[case::lambda_with_environment_2(
        "let \
            make_add_n = (x: int) -> (y: int) -> x + y, \
            add_1 = make_add_n(1), \
            add_2 = make_add_n(2) \
        in \
            add_1(1) * add_2(1)",
        Failure.without_modifiers(),
        6,
    )]
    #[case::rounding_to_zero("as_int(-2.99) * as_int(3.99)", Failure.without_modifiers(), -6)]
    fn evaluate_success(
        #[case] code: &str,
        #[case] outcome: SkillCheckOutcome,
        #[case] expected_value: impl Into<ExpectedValue>,
    ) {
        const EPSILON: f64 = 0.0001;

        let expression = parse_and_analyze(code);
        let result = evaluate(&expression, &outcome).unwrap();
        let expected_value = expected_value.into();

        match (result, expected_value) {
            (Value::Integer(value), ExpectedValue::Integer(expected_value)) => {
                assert_that!(value).is_equal_to(expected_value);
            },
            (Value::Float(value), ExpectedValue::Float(expected_value)) => {
                assert_that!(value).is_close_to(expected_value, EPSILON);
            },
            (value, expected_value) => panic!(
                "kind of value {:?} does not match expected value {:?}",
                value, expected_value
            ),
        }
    }

    #[rstest]
    #[case::add_overflow(
        "if true then 9223372036854775807 + 1 else 0",
        RuntimeErrorKind::ArithmeticOverflow,
        13..36,
    )]
    #[case::sub_overflow(
        "if false then 0 else -9223372036854775807 - 2",
        RuntimeErrorKind::ArithmeticOverflow,
        21..45,
    )]
    #[case::mul_overflow(
        "as_float(4294967296 * 4294967296)",
        RuntimeErrorKind::ArithmeticOverflow,
        9..32,
    )]
    #[case::neg_overflow(
        "let f = (x: int) -> -(-x - 1) in if f(9223372036854775807) > 0 then 1 else 0",
        RuntimeErrorKind::ArithmeticOverflow,
        20..29,
    )]
    #[case::div_by_zero("1 + 1 / 0", RuntimeErrorKind::DivideByZero, 4..9)]
    #[case::mod_by_zero("1 % 0 + 1", RuntimeErrorKind::DivideByZero, 0..5)]
    fn evaluate_error(
        #[case] code: &str,
        #[case] expected_kind: RuntimeErrorKind,
        #[case] expected_span: impl Into<CodeSpan>,
    ) {
        let expression = parse_and_analyze(code);
        let result = evaluate(&expression, &Failure.without_modifiers());

        assert_that!(result).contains_error(RuntimeError {
            kind: expected_kind,
            span: expected_span.into(),
        });
    }
}
