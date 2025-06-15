use std::fmt;
use std::fmt::Display;

use fmt::Formatter;
use thiserror::Error;

use crate::context::Type;
use crate::lexer::Token;
use crate::span::CodeSpan;

#[derive(Debug, Error, PartialEq)]
pub enum LexerErrorKind {
    #[error("integer literal exceeds upper bound: {0}")]
    IntegerOverflow(String),
    #[error("invalid character: {0}")]
    InvalidCharacter(char),
}

#[derive(Debug, Error, PartialEq)]
#[error("{kind} @ {span}")]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: CodeSpan,
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    // TODO list of expected tokens?
    #[error("unexpected token: {0}")]
    UnexpectedToken(Token),
}

#[derive(Debug, PartialEq)]
pub enum ExpectedType {
    Type(Type),
    AnyFunction,
}

impl Display for ExpectedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExpectedType::Type(typ) => write!(f, "{}", typ),
            ExpectedType::AnyFunction => write!(f, "<function>"),
        }
    }
}

impl From<Type> for ExpectedType {
    fn from(typ: Type) -> ExpectedType {
        ExpectedType::Type(typ)
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Error, PartialEq)]
pub enum ContextError {
    #[error("unresolved reference '{identifier}' @ {span}")]
    UnresolvedReference { identifier: String, span: CodeSpan },

    #[error("type error @ {span}: received {actual_type}, expected [ {expected_type_list} ]",
        expected_type_list = .expected_types.iter()
            .map(|expected_type| format!("{}", expected_type))
            .collect::<Vec<_>>()
            .join(", ")
    )]
    TypeError {
        actual_type: Type,
        expected_types: Vec<ExpectedType>,
        span: CodeSpan,
    },

    #[error(
        "function of type {callee_type} requires {parameter_count} arguments, but received \
            {argument_count} @ {span}"
    )]
    CardinalityError {
        callee_type: Type,
        parameter_count: usize,
        argument_count: usize,
        span: CodeSpan,
    },
}

pub type ContextResult<T> = Result<T, ContextError>;

#[derive(Debug, Error, PartialEq)]
pub enum InitializationError {
    #[error("{0}")]
    Lexer(#[from] LexerError),
    #[error("{0}")]
    Parse(#[from] ParseError),
    #[error("{0}")]
    Context(#[from] ContextError),
}

pub type InitializationResult<T> = Result<T, InitializationError>;

#[derive(Debug, Error)]
pub enum RuntimeErrorKind {
    #[error("arithmetic operation caused overflow")]
    ArithmeticOverflow,

    #[error("division by zero")]
    DivideByZero,

    #[error("invalid result: {0} (a finite number is required)")]
    InvalidResult(f64),
}

impl PartialEq for RuntimeErrorKind {
    fn eq(&self, other: &RuntimeErrorKind) -> bool {
        match (self, other) {
            (RuntimeErrorKind::ArithmeticOverflow, RuntimeErrorKind::ArithmeticOverflow) => true,
            (RuntimeErrorKind::DivideByZero, RuntimeErrorKind::DivideByZero) => true,
            (
                RuntimeErrorKind::InvalidResult(self_value),
                RuntimeErrorKind::InvalidResult(other_value),
            ) => self_value == other_value || self_value.is_nan() && other_value.is_nan(),
            _ => false,
        }
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("{kind} @ {span}")]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: CodeSpan,
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
