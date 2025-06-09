use thiserror::Error;

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

pub type ParseResult<T> = Result<T, ParseError>;
