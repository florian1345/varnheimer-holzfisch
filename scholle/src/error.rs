use thiserror::Error;

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
