use std::str::CharIndices;

use crate::error::{LexerError, LexerErrorKind, LexerResult};
use crate::span::CodeSpan;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    True,
    False,
    If,
    Then,
    Else,
    Let,
    In,
    Int,
    Float,
    Bool,
    LeftParenthesis,
    RightParenthesis,
    Comma,
    Colon,
    Assign,
    Arrow,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    Not,
    And,
    Or,
    Xor,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EndOfCode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: CodeSpan,
}

struct PeekableCharIndices<'a> {
    char_indices: CharIndices<'a>,
    peeked: Option<(usize, char)>,
}

impl<'a> PeekableCharIndices<'a> {
    fn new(char_indices: CharIndices<'a>) -> Self {
        PeekableCharIndices {
            char_indices,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        if self.peeked.is_none() {
            self.peeked = self.char_indices.next();
        }

        self.peeked
    }

    fn next_if(&mut self, predicate: impl FnOnce(char) -> bool) -> bool {
        if self.peek().is_some_and(|(_, c)| predicate(c)) {
            self.peeked.take();
            true
        }
        else {
            false
        }
    }

    fn next_if_eq(&mut self, required: char) -> bool {
        self.next_if(|c| c == required)
    }

    fn next_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while self.next_if(&mut predicate) {}
    }

    fn offset(&self) -> usize {
        self.peeked
            .map_or_else(|| self.char_indices.offset(), |(offset, _)| offset)
    }
}

impl<'a> Iterator for PeekableCharIndices<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.take().or_else(|| self.char_indices.next())
    }
}

fn is_identifier(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c.is_ascii_digit()
}

fn lex_token_kind(
    code: &str,
    char_indices: &mut PeekableCharIndices,
    start_byte: usize,
    first_char: char,
) -> Result<TokenKind, LexerErrorKind> {
    let token_kind = match first_char {
        '(' => TokenKind::LeftParenthesis,
        ')' => TokenKind::RightParenthesis,
        ',' => TokenKind::Comma,
        ':' => TokenKind::Colon,
        '=' => {
            if char_indices.next_if_eq('=') {
                TokenKind::Equal
            }
            else {
                TokenKind::Assign
            }
        },
        '+' => TokenKind::Addition,
        '-' => {
            if char_indices.next_if_eq('>') {
                TokenKind::Arrow
            }
            else {
                TokenKind::Subtraction
            }
        },
        '*' => TokenKind::Multiplication,
        '/' => TokenKind::Division,
        '%' => TokenKind::Modulo,
        '!' => {
            if char_indices.next_if_eq('=') {
                TokenKind::NotEqual
            }
            else {
                TokenKind::Not
            }
        },
        '&' => TokenKind::And,
        '|' => TokenKind::Or,
        '^' => TokenKind::Xor,
        '<' => {
            if char_indices.next_if_eq('=') {
                TokenKind::LessEqual
            }
            else {
                TokenKind::Less
            }
        },
        '>' => {
            if char_indices.next_if_eq('=') {
                TokenKind::GreaterEqual
            }
            else {
                TokenKind::Greater
            }
        },
        c if c.is_ascii_digit() => {
            char_indices.next_while(|c| c.is_ascii_digit());

            if char_indices.next_if_eq('.') {
                char_indices.next_while(|c| c.is_ascii_digit());
                TokenKind::FloatLiteral(code[start_byte..char_indices.offset()].parse().unwrap())
            }
            else {
                TokenKind::IntLiteral(code[start_byte..char_indices.offset()].parse().map_err(
                    |_| {
                        LexerErrorKind::IntegerOverflow(
                            code[start_byte..char_indices.offset()].to_owned(),
                        )
                    },
                )?)
            }
        },
        c if is_identifier(c) => {
            char_indices.next_while(is_identifier);

            match &code[start_byte..char_indices.offset()] {
                "true" => TokenKind::True,
                "false" => TokenKind::False,
                "if" => TokenKind::If,
                "then" => TokenKind::Then,
                "else" => TokenKind::Else,
                "let" => TokenKind::Let,
                "in" => TokenKind::In,
                "int" => TokenKind::Int,
                "float" => TokenKind::Float,
                "bool" => TokenKind::Bool,
                identifier => TokenKind::Identifier(identifier.to_owned()),
            }
        },
        c => return Err(LexerErrorKind::InvalidCharacter(c)),
    };

    Ok(token_kind)
}

pub fn lex(code: &str) -> LexerResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut char_indices = PeekableCharIndices::new(code.char_indices());

    while char_indices.peek().is_some() {
        char_indices.next_while(char::is_whitespace);

        let Some((start_byte, first_char)) = char_indices.next()
        else {
            break;
        };

        let token_kind_result = lex_token_kind(code, &mut char_indices, start_byte, first_char);

        let span = CodeSpan {
            start_byte,
            end_byte: char_indices.offset(),
        };

        match token_kind_result {
            Ok(kind) => tokens.push(Token { kind, span }),
            Err(kind) => return Err(LexerError { kind, span }),
        }
    }

    tokens.push(Token {
        kind: TokenKind::EndOfCode,
        span: CodeSpan {
            start_byte: code.len(),
            end_byte: code.len(),
        },
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    fn token(kind: TokenKind, start_byte: usize, end_byte: usize) -> Token {
        Token {
            kind,
            span: CodeSpan {
                start_byte,
                end_byte,
            },
        }
    }

    fn end_of_code(len: usize) -> Token {
        token(TokenKind::EndOfCode, len, len)
    }

    #[rstest]
    #[case("_")]
    #[case("_123")]
    #[case("abc123")]
    #[case("ABC123")]
    #[case("A_c_C")]
    #[case("____")]
    fn identifiers(#[case] text: &str) {
        let tokens = lex(text).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::Identifier(text.to_owned()), 0, text.len()),
            end_of_code(text.len()),
        ]);
    }

    #[rstest]
    #[case("true", TokenKind::True)]
    #[case("false", TokenKind::False)]
    #[case("if", TokenKind::If)]
    #[case("then", TokenKind::Then)]
    #[case("else", TokenKind::Else)]
    #[case("let", TokenKind::Let)]
    #[case("in", TokenKind::In)]
    #[case("int", TokenKind::Int)]
    #[case("float", TokenKind::Float)]
    #[case("bool", TokenKind::Bool)]
    fn keywords(#[case] text: &str, #[case] expected_kind: TokenKind) {
        let tokens = lex(text).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(expected_kind, 0, text.len()),
            end_of_code(text.len()),
        ]);
    }

    #[test]
    fn identifiers_split_by_spaces() {
        let code = "\tel se\nflo \r at ";

        let tokens = lex(code).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::Identifier("el".to_owned()), 1, 3),
            token(TokenKind::Identifier("se".to_owned()), 4, 6),
            token(TokenKind::Identifier("flo".to_owned()), 7, 10),
            token(TokenKind::Identifier("at".to_owned()), 13, 15),
            end_of_code(16),
        ]);
    }

    #[test]
    fn simple_single_character_tokens() {
        let code = "(),:+*/%&|^";

        let tokens = lex(code).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::LeftParenthesis, 0, 1),
            token(TokenKind::RightParenthesis, 1, 2),
            token(TokenKind::Comma, 2, 3),
            token(TokenKind::Colon, 3, 4),
            token(TokenKind::Addition, 4, 5),
            token(TokenKind::Multiplication, 5, 6),
            token(TokenKind::Division, 6, 7),
            token(TokenKind::Modulo, 7, 8),
            token(TokenKind::And, 8, 9),
            token(TokenKind::Or, 9, 10),
            token(TokenKind::Xor, 10, 11),
            end_of_code(11),
        ]);
    }

    #[rstest]
    #[case("==", TokenKind::Equal)]
    #[case("!=", TokenKind::NotEqual)]
    #[case("<=", TokenKind::LessEqual)]
    #[case(">=", TokenKind::GreaterEqual)]
    #[case("->", TokenKind::Arrow)]
    fn double_character_tokens_joined(#[case] code: &str, #[case] expected_kind: TokenKind) {
        let tokens = lex(code).unwrap();

        assert_that!(tokens)
            .contains_exactly_in_given_order([token(expected_kind, 0, 2), end_of_code(2)]);
    }

    #[rstest]
    #[case("= =", TokenKind::Assign, TokenKind::Assign)]
    #[case("!\t=", TokenKind::Not, TokenKind::Assign)]
    #[case("<\n=", TokenKind::Less, TokenKind::Assign)]
    #[case(">\r=", TokenKind::Greater, TokenKind::Assign)]
    #[case("- >", TokenKind::Subtraction, TokenKind::Greater)]
    fn double_character_tokens_separated_by_whitespace(
        #[case] code: &str,
        #[case] expected_first_kind: TokenKind,
        #[case] expected_second_kind: TokenKind,
    ) {
        let tokens = lex(code).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(expected_first_kind, 0, 1),
            token(expected_second_kind, 2, 3),
            end_of_code(3),
        ]);
    }

    #[rstest]
    #[case("0", 0)]
    #[case("9", 9)]
    #[case("09", 9)]
    #[case("1234567890", 1234567890)]
    #[case("9223372036854775807", i64::MAX)]
    fn integer_literals(#[case] code: &str, #[case] expected_value: i64) {
        let tokens = lex(code).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::IntLiteral(expected_value), 0, code.len()),
            end_of_code(code.len()),
        ]);
    }

    #[test]
    fn integer_literals_split_by_whitespace() {
        let tokens = lex("123 456").unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::IntLiteral(123), 0, 3),
            token(TokenKind::IntLiteral(456), 4, 7),
            end_of_code(7),
        ]);
    }

    #[rstest]
    #[case("0.", 0.0)]
    #[case("0.0", 0.0)]
    #[case("0.01", 0.01)]
    #[case("12345.67890", 12345.6789)]
    #[case("9223372036854775808.0", 9223372036854775808.0)]
    fn float_literals(#[case] code: &str, #[case] expected_value: f64) {
        let tokens = lex(code).unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::FloatLiteral(expected_value), 0, code.len()),
            end_of_code(code.len()),
        ]);
    }

    #[test]
    fn float_literal_separated_by_space() {
        let tokens = lex("123. 456").unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::FloatLiteral(123.0), 0, 4),
            token(TokenKind::IntLiteral(456), 5, 8),
            end_of_code(8),
        ]);
    }

    #[test]
    fn identifier_and_numbers_separated_by_other_tokens() {
        let tokens = lex("abc=def+123-456").unwrap();

        assert_that!(tokens).contains_exactly_in_given_order([
            token(TokenKind::Identifier("abc".to_owned()), 0, 3),
            token(TokenKind::Assign, 3, 4),
            token(TokenKind::Identifier("def".to_owned()), 4, 7),
            token(TokenKind::Addition, 7, 8),
            token(TokenKind::IntLiteral(123), 8, 11),
            token(TokenKind::Subtraction, 11, 12),
            token(TokenKind::IntLiteral(456), 12, 15),
            end_of_code(15),
        ]);
    }

    #[test]
    fn lexer_error_integer_overflow() {
        let result = lex("   9223372036854775808   asd");

        assert_that!(result).contains_error(LexerError {
            kind: LexerErrorKind::IntegerOverflow("9223372036854775808".to_owned()),
            span: CodeSpan {
                start_byte: 3,
                end_byte: 22,
            },
        });
    }

    #[rstest]
    #[case('#')]
    #[case('~')]
    #[case('.')]
    #[case::non_ascii('Ä')]
    #[case::non_bmp('🎉')]
    fn lexer_error_invalid_character(#[case] invalid_char: char) {
        let result = lex(&String::from(invalid_char));

        assert_that!(result).contains_error(LexerError {
            kind: LexerErrorKind::InvalidCharacter(invalid_char),
            span: CodeSpan {
                start_byte: 0,
                end_byte: invalid_char.len_utf8(),
            },
        });
    }
}
