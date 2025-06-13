use std::collections::VecDeque;

use crate::error::{ParseError, ParseResult};
use crate::lexer::{Token, TokenKind};
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::span::CodeSpan;

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub identifier: String,
    pub value: Expression,
    pub span: CodeSpan,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeKind {
    Integer,
    Float,
    Bool,
    Function {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub span: CodeSpan,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub typ: Type,
    pub span: CodeSpan,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Reference(String),
    UnaryOperation {
        operand: Box<Expression>,
        operator: UnaryOperator,
    },
    BinaryOperation {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        operator: BinaryOperator,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    Let {
        assignments: Vec<Assignment>,
        body: Box<Expression>,
    },
    Lambda {
        parameters: Vec<Parameter>,
        body: Box<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: CodeSpan,
}

struct Parser<IterT> {
    iter: IterT,
    lookahead_buffer: VecDeque<Token>,
}

impl<IterT> Parser<IterT> {
    fn new(iter: impl IntoIterator<IntoIter = IterT>) -> Parser<IterT> {
        Parser {
            iter: iter.into_iter(),
            lookahead_buffer: VecDeque::with_capacity(2),
        }
    }
}

const NO_EOC_MESSAGE: &str = "tokens did not contain end-of-code token";

impl<IterT: Iterator<Item = Token>> Parser<IterT> {
    fn take_it(&mut self) -> Token {
        self.next().expect(NO_EOC_MESSAGE)
    }

    fn take_with(&mut self, requirement: impl FnOnce(&TokenKind) -> bool) -> ParseResult<Token> {
        let token = self.take_it();

        if requirement(&token.kind) {
            Ok(token)
        }
        else {
            Err(ParseError::UnexpectedToken(token))
        }
    }

    fn take(&mut self, required_kind: TokenKind) -> ParseResult<Token> {
        self.take_with(|kind| kind == &required_kind)
    }

    fn take_identifier(&mut self) -> ParseResult<(String, CodeSpan)> {
        let identifier_token = self.take_it();

        match identifier_token.kind {
            TokenKind::Identifier(identifier) => Ok((identifier, identifier_token.span)),
            _ => Err(ParseError::UnexpectedToken(identifier_token)),
        }
    }

    fn take_if(&mut self, required_kind: TokenKind) -> bool {
        if self.peek().kind == required_kind {
            self.take_it();
            true
        }
        else {
            false
        }
    }

    /// Same as `lookahead`, but assumes that `lookahead(offset - 1)` is already known not to be
    /// end-of-code.
    fn lookahead(&mut self, offset: usize) -> &Token {
        while self.lookahead_buffer.len() <= offset {
            self.lookahead_buffer
                .push_back(self.iter.next().expect(NO_EOC_MESSAGE));
        }

        self.lookahead_buffer.get(offset).unwrap()
    }

    fn peek(&mut self) -> &Token {
        self.lookahead(0)
    }

    fn peek_operator<T: Copy>(&mut self, token_kinds_to_operator: &[(TokenKind, T)]) -> Option<T> {
        let next_token_kind = &self.peek().kind;
        token_kinds_to_operator
            .iter()
            .find(|(kind, _)| kind == next_token_kind)
            .map(|(_, operator)| *operator)
    }

    fn check_end(&mut self) -> ParseResult<()> {
        self.take(TokenKind::EndOfCode)?;

        if self.next().is_some() {
            panic!("found tokens after end-of-code token");
        }

        Ok(())
    }

    fn parse_assignment(&mut self) -> ParseResult<Assignment> {
        let (identifier, identifier_span) = self.take_identifier()?;
        self.take(TokenKind::Assign)?;
        let value = self.parse_or()?;
        let span = identifier_span.union(value.span);

        Ok(Assignment {
            identifier,
            value,
            span,
        })
    }

    fn continue_parsing_parenthesized_list<ItemT>(
        &mut self,
        parse_item: impl Fn(&mut Parser<IterT>) -> ParseResult<ItemT>,
    ) -> ParseResult<(Vec<ItemT>, Token)> {
        let mut items = Vec::new();

        if self.peek().kind != TokenKind::RightParenthesis {
            items.push(parse_item(self)?);

            while self.take_if(TokenKind::Comma) {
                items.push(parse_item(self)?);
            }
        }

        Ok((items, self.take(TokenKind::RightParenthesis)?))
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let first_token = self.take_it();

        match first_token.kind {
            TokenKind::Int => Ok(Type {
                kind: TypeKind::Integer,
                span: first_token.span,
            }),
            TokenKind::Float => Ok(Type {
                kind: TypeKind::Float,
                span: first_token.span,
            }),
            TokenKind::Bool => Ok(Type {
                kind: TypeKind::Bool,
                span: first_token.span,
            }),
            TokenKind::LeftParenthesis => {
                let (parameter_types, _) =
                    self.continue_parsing_parenthesized_list(Parser::parse_type)?;

                self.take(TokenKind::Arrow)?;

                let return_type = self.parse_type()?;
                let span = first_token.span.union(return_type.span);

                Ok(Type {
                    kind: TypeKind::Function {
                        parameter_types,
                        return_type: Box::new(return_type),
                    },
                    span,
                })
            },
            token_kind => Err(ParseError::UnexpectedToken(Token {
                kind: token_kind,
                span: first_token.span,
            })),
        }
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
        let (identifier, identifier_span) = self.take_identifier()?;
        self.take(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        let span = identifier_span.union(typ.span);

        Ok(Parameter {
            identifier,
            typ,
            span,
        })
    }

    fn parse_atomic(&mut self) -> ParseResult<Expression> {
        let first_token = self.take_it();

        match first_token.kind {
            TokenKind::Identifier(identifier) => Ok(Expression {
                kind: ExpressionKind::Reference(identifier),
                span: first_token.span,
            }),
            TokenKind::IntLiteral(value) => Ok(Expression {
                kind: ExpressionKind::IntLiteral(value),
                span: first_token.span,
            }),
            TokenKind::FloatLiteral(value) => Ok(Expression {
                kind: ExpressionKind::FloatLiteral(value),
                span: first_token.span,
            }),
            kind @ (TokenKind::True | TokenKind::False) => Ok(Expression {
                kind: ExpressionKind::BoolLiteral(kind == TokenKind::True),
                span: first_token.span,
            }),
            TokenKind::If => {
                let condition = self.parse_or()?;
                self.take(TokenKind::Then)?;
                let then_branch = self.parse_or()?;
                self.take(TokenKind::Else)?;
                let else_branch = self.parse_or()?;
                let span = first_token.span.union(else_branch.span);

                Ok(Expression {
                    kind: ExpressionKind::If {
                        condition: Box::new(condition),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    },
                    span,
                })
            },
            TokenKind::Let => {
                let mut assignments = vec![self.parse_assignment()?];

                while self.take_if(TokenKind::Comma) {
                    assignments.push(self.parse_assignment()?);
                }

                self.take(TokenKind::In)?;

                let body = self.parse_or()?;
                let span = first_token.span.union(body.span);

                Ok(Expression {
                    kind: ExpressionKind::Let {
                        assignments,
                        body: Box::new(body),
                    },
                    span,
                })
            },
            TokenKind::LeftParenthesis => {
                let next_token_kind = &self.peek().kind;
                let is_lambda = next_token_kind == &TokenKind::RightParenthesis
                    || matches!(next_token_kind, TokenKind::Identifier(_))
                        && self.lookahead(1).kind == TokenKind::Colon;

                if is_lambda {
                    let (parameters, _) =
                        self.continue_parsing_parenthesized_list(Parser::parse_parameter)?;

                    self.take(TokenKind::Arrow)?;

                    let body = self.parse_or()?;
                    let span = first_token.span.union(body.span);

                    Ok(Expression {
                        kind: ExpressionKind::Lambda {
                            parameters,
                            body: Box::new(body),
                        },
                        span,
                    })
                }
                else {
                    let parenthesized_expression = self.parse_or()?;
                    let end_token = self.take(TokenKind::RightParenthesis)?;

                    Ok(Expression {
                        kind: parenthesized_expression.kind,
                        span: first_token.span.union(end_token.span),
                    })
                }
            },
            token_kind => Err(ParseError::UnexpectedToken(Token {
                kind: token_kind,
                span: first_token.span,
            })),
        }
    }

    fn parse_call(&mut self) -> ParseResult<Expression> {
        let mut expression = self.parse_atomic()?;

        while self.take_if(TokenKind::LeftParenthesis) {
            let (arguments, right_parenthesis) =
                self.continue_parsing_parenthesized_list(Parser::parse_or)?;
            let span = expression.span.union(right_parenthesis.span);

            expression = Expression {
                kind: ExpressionKind::Call {
                    callee: Box::new(expression),
                    arguments,
                },
                span,
            };
        }

        Ok(expression)
    }

    fn parse_unary(&mut self) -> ParseResult<Expression> {
        const TOKENS_TO_UNARY_OPERATOR: [(TokenKind, UnaryOperator); 2] = [
            (TokenKind::Not, UnaryOperator::Not),
            (TokenKind::Subtraction, UnaryOperator::Negative),
        ];

        let mut operators = Vec::new();

        while let Some(operator) = self.peek_operator(&TOKENS_TO_UNARY_OPERATOR) {
            let token = self.take_it();
            operators.push((operator, token));
        }

        let mut expression = self.parse_call()?;

        while let Some((operator, token)) = operators.pop() {
            let span = token.span.union(expression.span);

            expression = Expression {
                kind: ExpressionKind::UnaryOperation {
                    operand: Box::new(expression),
                    operator,
                },
                span,
            };
        }

        Ok(expression)
    }

    fn parse_binary(
        &mut self,
        token_kinds_to_operator: &[(TokenKind, BinaryOperator)],
        parse_with_higher_precedence: impl Fn(&mut Parser<IterT>) -> ParseResult<Expression>,
    ) -> ParseResult<Expression> {
        let mut expression = parse_with_higher_precedence(self)?;

        while let Some(operator) = self.peek_operator(token_kinds_to_operator) {
            self.take_it();
            let rhs = parse_with_higher_precedence(self)?;
            let span = expression.span.union(rhs.span);
            expression = Expression {
                kind: ExpressionKind::BinaryOperation {
                    lhs: Box::new(expression),
                    rhs: Box::new(rhs),
                    operator,
                },
                span,
            };
        }

        Ok(expression)
    }

    fn parse_multiplicative(&mut self) -> ParseResult<Expression> {
        self.parse_binary(
            &[
                (TokenKind::Multiplication, BinaryOperator::Multiplication),
                (TokenKind::Division, BinaryOperator::Division),
                (TokenKind::Modulo, BinaryOperator::Modulo),
            ],
            Parser::parse_unary,
        )
    }

    fn parse_additive(&mut self) -> ParseResult<Expression> {
        self.parse_binary(
            &[
                (TokenKind::Addition, BinaryOperator::Addition),
                (TokenKind::Subtraction, BinaryOperator::Subtraction),
            ],
            Parser::parse_multiplicative,
        )
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
        self.parse_binary(
            &[
                (TokenKind::Less, BinaryOperator::Less),
                (TokenKind::LessEqual, BinaryOperator::LessEqual),
                (TokenKind::Greater, BinaryOperator::Greater),
                (TokenKind::GreaterEqual, BinaryOperator::GreaterEqual),
            ],
            Parser::parse_additive,
        )
    }

    fn parse_equality(&mut self) -> ParseResult<Expression> {
        self.parse_binary(
            &[
                (TokenKind::Equal, BinaryOperator::Equal),
                (TokenKind::NotEqual, BinaryOperator::NotEqual),
            ],
            Parser::parse_comparison,
        )
    }

    fn parse_and(&mut self) -> ParseResult<Expression> {
        self.parse_binary(
            &[(TokenKind::And, BinaryOperator::And)],
            Parser::parse_equality,
        )
    }

    fn parse_xor(&mut self) -> ParseResult<Expression> {
        self.parse_binary(&[(TokenKind::Xor, BinaryOperator::Xor)], Parser::parse_and)
    }

    fn parse_or(&mut self) -> ParseResult<Expression> {
        self.parse_binary(&[(TokenKind::Or, BinaryOperator::Or)], Parser::parse_xor)
    }
}

impl<IterT: Iterator<Item = Token>> Iterator for Parser<IterT> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.lookahead_buffer
            .pop_front()
            .or_else(|| self.iter.next())
    }
}

/// Parses a list of tokens into a Scholle-Expression. The tokens _must_ contain exactly one
/// [TokenKind::EndOfCode]-Token and _must not_ contain any tokens after it. If this condition is
/// violated, the function panics. All other syntax errors are reported as [ParseError]s.
pub fn parse(tokens: impl IntoIterator<Item = Token>) -> ParseResult<Expression> {
    let mut parser = Parser::new(tokens);

    let expression = parser.parse_or()?;
    parser.check_end()?;

    Ok(expression)
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use kernal::prelude::*;
    use rstest::rstest;

    use super::ExpressionKind::*;
    use super::TypeKind::*;
    use super::*;
    use crate::lexer;

    impl ExpressionKind {
        fn at(self, span_range: Range<usize>) -> Expression {
            Expression {
                kind: self,
                span: span_range.into(),
            }
        }

        fn at_boxed(self, span_range: Range<usize>) -> Box<Expression> {
            Box::new(self.at(span_range))
        }
    }

    impl TypeKind {
        fn at(self, span_range: Range<usize>) -> Type {
            Type {
                kind: self,
                span: span_range.into(),
            }
        }

        fn at_boxed(self, span_range: Range<usize>) -> Box<Type> {
            Box::new(self.at(span_range))
        }
    }

    #[rstest]
    #[case::identifier("abc", Reference("abc".to_owned()).at(0..3))]
    #[case::int_literal("100", IntLiteral(100).at(0..3))]
    #[case::float_literal("123.456", FloatLiteral(123.456).at(0..7))]
    #[case::bool_true("true", BoolLiteral(true).at(0..4))]
    #[case::bool_false("false", BoolLiteral(false).at(0..5))]
    #[case::parenthesized("(1)", IntLiteral(1).at(0..3))]
    #[case::addition("3 + a",
        BinaryOperation {
            lhs: IntLiteral(3).at_boxed(0..1),
            rhs: Reference("a".to_owned()).at_boxed(4..5),
            operator: BinaryOperator::Addition,
        }.at(0..5)
    )]
    #[case::subtraction("x - y",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: Reference("y".to_owned()).at_boxed(4..5),
            operator: BinaryOperator::Subtraction,
        }.at(0..5)
    )]
    #[case::addition_and_subtraction_left_to_right("a + b - c + d",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(0..1),
                    rhs: Reference("b".to_owned()).at_boxed(4..5),
                    operator: BinaryOperator::Addition,
                }.at_boxed(0..5),
                rhs: Reference("c".to_owned()).at_boxed(8..9),
                operator: BinaryOperator::Subtraction,
            }.at_boxed(0..9),
            rhs: Reference("d".to_owned()).at_boxed(12..13),
            operator: BinaryOperator::Addition,
        }.at(0..13)
    )]
    #[case::multiplication("2 * 2",
        BinaryOperation {
            lhs: IntLiteral(2).at_boxed(0..1),
            rhs: IntLiteral(2).at_boxed(4..5),
            operator: BinaryOperator::Multiplication,
        }.at(0..5)
    )]
    #[case::division("3.0 / 2.0",
        BinaryOperation {
            lhs: FloatLiteral(3.0).at_boxed(0..3),
            rhs: FloatLiteral(2.0).at_boxed(6..9),
            operator: BinaryOperator::Division,
        }.at(0..9)
    )]
    #[case::modulo("x % 2",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(2).at_boxed(4..5),
            operator: BinaryOperator::Modulo,
        }.at(0..5)
    )]
    #[case::multiplication_and_division_left_to_right("a * b / c * d",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(0..1),
                    rhs: Reference("b".to_owned()).at_boxed(4..5),
                    operator: BinaryOperator::Multiplication,
                }.at_boxed(0..5),
                rhs: Reference("c".to_owned()).at_boxed(8..9),
                operator: BinaryOperator::Division,
            }.at_boxed(0..9),
            rhs: Reference("d".to_owned()).at_boxed(12..13),
            operator: BinaryOperator::Multiplication,
        }.at(0..13)
    )]
    #[case::multiplication_before_addition("a + 2 * b",
        BinaryOperation {
            lhs: Reference("a".to_owned()).at_boxed(0..1),
            rhs: BinaryOperation {
                lhs: IntLiteral(2).at_boxed(4..5),
                rhs: Reference("b".to_owned()).at_boxed(8..9),
                operator: BinaryOperator::Multiplication,
            }.at_boxed(4..9),
            operator: BinaryOperator::Addition,
        }.at(0..9)
    )]
    #[case::parenthesized_addition_before_multiplication_left("(a + 2) * b",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: Reference("a".to_owned()).at_boxed(1..2),
                rhs: IntLiteral(2).at_boxed(5..6),
                operator: BinaryOperator::Addition,
            }.at_boxed(0..7),
            rhs: Reference("b".to_owned()).at_boxed(10..11),
            operator: BinaryOperator::Multiplication,
        }.at(0..11)
    )]
    #[case::parenthesized_addition_before_multiplication_right("b * (a + 2)",
        BinaryOperation {
            lhs: Reference("b".to_owned()).at_boxed(0..1),
            rhs: BinaryOperation {
                lhs: Reference("a".to_owned()).at_boxed(5..6),
                rhs: IntLiteral(2).at_boxed(9..10),
                operator: BinaryOperator::Addition,
            }.at_boxed(4..11),
            operator: BinaryOperator::Multiplication,
        }.at(0..11)
    )]
    #[case::or("x | y",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: Reference("y".to_owned()).at_boxed(4..5),
            operator: BinaryOperator::Or,
        }.at(0..5)
    )]
    #[case::xor("true ^ false",
        BinaryOperation {
            lhs: BoolLiteral(true).at_boxed(0..4),
            rhs: BoolLiteral(false).at_boxed(7..12),
            operator: BinaryOperator::Xor,
        }.at(0..12)
    )]
    #[case::and("a & b",
        BinaryOperation {
            lhs: Reference("a".to_owned()).at_boxed(0..1),
            rhs: Reference("b".to_owned()).at_boxed(4..5),
            operator: BinaryOperator::And,
        }.at(0..5)
    )]
    #[case::logical_operators_precedence("a | b ^ c & d",
        BinaryOperation {
            lhs: Reference("a".to_owned()).at_boxed(0..1),
            rhs: BinaryOperation {
                lhs: Reference("b".to_owned()).at_boxed(4..5),
                rhs: BinaryOperation {
                    lhs: Reference("c".to_owned()).at_boxed(8..9),
                    rhs: Reference("d".to_owned()).at_boxed(12..13),
                    operator: BinaryOperator::And,
                }.at_boxed(8..13),
                operator: BinaryOperator::Xor,
            }.at_boxed(4..13),
            operator: BinaryOperator::Or,
        }.at(0..13)
    )]
    #[case::logical_operators_parentheses_left("((a | b) ^ c) & d",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(2..3),
                    rhs: Reference("b".to_owned()).at_boxed(6..7),
                    operator: BinaryOperator::Or,
                }.at_boxed(1..8),
                rhs: Reference("c".to_owned()).at_boxed(11..12),
                operator: BinaryOperator::Xor,
            }.at_boxed(0..13),
            rhs: Reference("d".to_owned()).at_boxed(16..17),
            operator: BinaryOperator::And,
        }.at(0..17)
    )]
    #[case::equals("x == 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(5..6),
            operator: BinaryOperator::Equal,
        }.at(0..6)
    )]
    #[case::not_equal("x != 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(5..6),
            operator: BinaryOperator::NotEqual,
        }.at(0..6)
    )]
    #[case::less("x < 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(4..5),
            operator: BinaryOperator::Less,
        }.at(0..5)
    )]
    #[case::less_equal("x <= 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(5..6),
            operator: BinaryOperator::LessEqual,
        }.at(0..6)
    )]
    #[case::greater("x > 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(4..5),
            operator: BinaryOperator::Greater,
        }.at(0..5)
    )]
    #[case::greater_equal("x >= 5",
        BinaryOperation {
            lhs: Reference("x".to_owned()).at_boxed(0..1),
            rhs: IntLiteral(5).at_boxed(5..6),
            operator: BinaryOperator::GreaterEqual,
        }.at(0..6)
    )]
    #[case::comparison_precedence_over_equality("a > 1 == b < 2 != c >= 3",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(0..1),
                    rhs: IntLiteral(1).at_boxed(4..5),
                    operator: BinaryOperator::Greater,
                }.at_boxed(0..5),
                rhs: BinaryOperation {
                    lhs: Reference("b".to_owned()).at_boxed(9..10),
                    rhs: IntLiteral(2).at_boxed(13..14),
                    operator: BinaryOperator::Less,
                }.at_boxed(9..14),
                operator: BinaryOperator::Equal,
            }.at_boxed(0..14),
            rhs: BinaryOperation {
                lhs: Reference("c".to_owned()).at_boxed(18..19),
                rhs: IntLiteral(3).at_boxed(23..24),
                operator: BinaryOperator::GreaterEqual,
            }.at_boxed(18..24),
            operator: BinaryOperator::NotEqual,
        }.at(0..24)
    )]
    #[case::comparison_equality_parentheses("a > (1 == b) < (2 != c) >= 3",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(0..1),
                    rhs: BinaryOperation {
                        lhs: IntLiteral(1).at_boxed(5..6),
                        rhs: Reference("b".to_owned()).at_boxed(10..11),
                        operator: BinaryOperator::Equal,
                    }.at_boxed(4..12),
                    operator: BinaryOperator::Greater,
                }.at_boxed(0..12),
                rhs: BinaryOperation {
                    lhs: IntLiteral(2).at_boxed(16..17),
                    rhs: Reference("c".to_owned()).at_boxed(21..22),
                    operator: BinaryOperator::NotEqual,
                }.at_boxed(15..23),
                operator: BinaryOperator::Less,
            }.at_boxed(0..23),
            rhs: IntLiteral(3).at_boxed(27..28),
            operator: BinaryOperator::GreaterEqual,
        }.at(0..28)
    )]
    #[case::arithmetic_comparison_logic_precedence("a + 1 > b & c != d * 2",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference("a".to_owned()).at_boxed(0..1),
                    rhs: IntLiteral(1).at_boxed(4..5),
                    operator: BinaryOperator::Addition,
                }.at_boxed(0..5),
                rhs: Reference("b".to_owned()).at_boxed(8..9),
                operator: BinaryOperator::Greater,
            }.at_boxed(0..9),
            rhs: BinaryOperation {
                lhs: Reference("c".to_owned()).at_boxed(12..13),
                rhs: BinaryOperation {
                    lhs: Reference("d".to_owned()).at_boxed(17..18),
                    rhs: IntLiteral(2).at_boxed(21..22),
                    operator: BinaryOperator::Multiplication,
                }.at_boxed(17..22),
                operator: BinaryOperator::NotEqual,
            }.at_boxed(12..22),
            operator: BinaryOperator::And,
        }.at(0..22)
    )]
    #[case::not("!x",
        UnaryOperation {
            operand: Reference("x".to_owned()).at_boxed(1..2),
            operator: UnaryOperator::Not,
        }.at(0..2)
    )]
    #[case::negative("-1",
        UnaryOperation {
            operand: IntLiteral(1).at_boxed(1..2),
            operator: UnaryOperator::Negative,
        }.at(0..2)
    )]
    #[case::double_unary_operator("--1.5",
        UnaryOperation {
            operand: UnaryOperation {
                operand: FloatLiteral(1.5).at_boxed(2..5),
                operator: UnaryOperator::Negative,
            }.at_boxed(1..5),
            operator: UnaryOperator::Negative,
        }.at(0..5)
    )]
    #[case::unary_precedence("a / -b",
        BinaryOperation {
            lhs: Reference("a".to_owned()).at_boxed(0..1),
            rhs: UnaryOperation {
                operand: Reference("b".to_owned()).at_boxed(5..6),
                operator: UnaryOperator::Negative,
            }.at_boxed(4..6),
            operator: BinaryOperator::Division,
        }.at(0..6)
    )]
    #[case::empty_function_call("fn()",
        Call {
            callee: Reference("fn".to_owned()).at_boxed(0..2),
            arguments: vec![],
        }.at(0..4)
    )]
    #[case::function_call_with_single_argument("sqrt(2.0)",
        Call {
            callee: Reference("sqrt".to_owned()).at_boxed(0..4),
            arguments: vec![
                FloatLiteral(2.0).at(5..8)
            ],
        }.at(0..9)
    )]
    #[case::function_call_with_two_arguments("pow(x, 1.5)",
        Call {
            callee: Reference("pow".to_owned()).at_boxed(0..3),
            arguments: vec![
                Reference("x".to_owned()).at(4..5),
                FloatLiteral(1.5).at(7..10),
            ],
        }.at(0..11)
    )]
    #[case::negated_function_call("-f(x)",
        UnaryOperation {
            operand: Call {
                callee: Reference("f".to_owned()).at_boxed(1..2),
                arguments: vec![
                    Reference("x".to_owned()).at(3..4),
                ],
            }.at_boxed(1..5),
            operator: UnaryOperator::Negative,
        }.at(0..5)
    )]
    #[case::curried_function_call("f(x)()",
        Call {
            callee: Call {
                callee: Reference("f".to_owned()).at_boxed(0..1),
                arguments: vec![
                    Reference("x".to_owned()).at(2..3),
                ],
            }.at_boxed(0..4),
            arguments: vec![],
        }.at(0..6)
    )]
    #[case::simple_if("if x then y else z",
        If {
            condition: Reference("x".to_owned()).at_boxed(3..4),
            then_branch: Reference("y".to_owned()).at_boxed(10..11),
            else_branch: Reference("z".to_owned()).at_boxed(17..18),
        }.at(0..18)
    )]
    #[case::nested_if("if a then if b then c else d else if e then f else g",
        If {
            condition: Reference("a".to_owned()).at_boxed(3..4),
            then_branch: If {
                condition: Reference("b".to_owned()).at_boxed(13..14),
                then_branch: Reference("c".to_owned()).at_boxed(20..21),
                else_branch: Reference("d".to_owned()).at_boxed(27..28),
            }.at_boxed(10..28),
            else_branch: If {
                condition: Reference("e".to_owned()).at_boxed(37..38),
                then_branch: Reference("f".to_owned()).at_boxed(44..45),
                else_branch: Reference("g".to_owned()).at_boxed(51..52),
            }.at_boxed(34..52),
        }.at(0..52)
    )]
    #[case::let_single_assignment("let x = 5 in x",
        Let {
            assignments: vec![
                Assignment {
                    identifier: "x".to_owned(),
                    value: IntLiteral(5).at(8..9),
                    span: (4..9).into(),
                },
            ],
            body: Reference("x".to_owned()).at_boxed(13..14),
        }.at(0..14)
    )]
    #[case::let_multiple_assignments("let x = 3, y = x * x in y",
        Let {
            assignments: vec![
                Assignment {
                    identifier: "x".to_owned(),
                    value: IntLiteral(3).at(8..9),
                    span: (4..9).into(),
                },
                Assignment {
                    identifier: "y".to_owned(),
                    value: BinaryOperation {
                        lhs: Reference("x".to_owned()).at_boxed(15..16),
                        rhs: Reference("x".to_owned()).at_boxed(19..20),
                        operator: BinaryOperator::Multiplication,
                    }.at(15..20),
                    span: (11..20).into(),
                },
            ],
            body: Reference("y".to_owned()).at_boxed(24..25),
        }.at(0..25)
    )]
    #[case::lambda_without_parameter("() -> false",
        Lambda {
            parameters: vec![],
            body: BoolLiteral(false).at_boxed(6..11),
        }.at(0..11)
    )]
    #[case::lambda_with_int_parameter("(x: int) -> x * x",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "x".to_owned(),
                    typ: Integer.at(4..7),
                    span: (1..7).into(),
                }
            ],
            body: BinaryOperation {
                lhs: Reference("x".to_owned()).at_boxed(12..13),
                rhs: Reference("x".to_owned()).at_boxed(16..17),
                operator: BinaryOperator::Multiplication,
            }.at_boxed(12..17),
        }.at(0..17)
    )]
    #[case::lambda_with_bool_and_float_parameter(
        "(double: bool, x: float) -> if double then 2.0 * x else x",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "double".to_owned(),
                    typ: Bool.at(9..13),
                    span: (1..13).into(),
                },
                Parameter {
                    identifier: "x".to_owned(),
                    typ: Float.at(18..23),
                    span: (15..23).into(),
                }
            ],
            body: If {
                condition: Reference("double".to_owned()).at_boxed(31..37),
                then_branch: BinaryOperation {
                    lhs: FloatLiteral(2.0).at_boxed(43..46),
                    rhs: Reference("x".to_owned()).at_boxed(49..50),
                    operator: BinaryOperator::Multiplication,
                }.at_boxed(43..50),
                else_branch: Reference("x".to_owned()).at_boxed(56..57),
            }.at_boxed(28..57),
        }.at(0..57)
    )]
    #[case::lambda_with_function_parameter("(f: (int) -> int) -> f(0)",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "f".to_owned(),
                    typ: Function {
                        parameter_types: vec![Integer.at(5..8)],
                        return_type: Integer.at_boxed(13..16),
                    }.at(4..16),
                    span: (1..16).into(),
                },
            ],
            body: Call {
                callee: Reference("f".to_owned()).at_boxed(21..22),
                arguments: vec![IntLiteral(0).at(23..24)]
            }.at_boxed(21..25),
        }.at(0..25)
    )]
    #[case::lambda_returning_function("(factor: float) -> (x: float) -> factor * x",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "factor".to_owned(),
                    typ: Float.at(9..14),
                    span: (1..14).into(),
                }
            ],
            body: Lambda {
                parameters: vec![
                    Parameter {
                        identifier: "x".to_owned(),
                        typ: Float.at(23..28),
                        span: (20..28).into(),
                    }
                ],
                body: BinaryOperation {
                    lhs: Reference("factor".to_owned()).at_boxed(33..39),
                    rhs: Reference("x".to_owned()).at_boxed(42..43),
                    operator: BinaryOperator::Multiplication,
                }.at_boxed(33..43),
            }.at_boxed(19..43),
        }.at(0..43)
    )]
    #[case::lambda_taking_function_with_two_arguments("(f: (int, bool) -> int) -> f(0, true)",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "f".to_owned(),
                    typ: Function {
                        parameter_types: vec![
                            Integer.at(5..8),
                            Bool.at(10..14),
                        ],
                        return_type: Integer.at_boxed(19..22),
                    }.at(4..22),
                    span: (1..22).into(),
                }
            ],
            body: Call {
                callee: Reference("f".to_owned()).at_boxed(27..28),
                arguments: vec![
                    IntLiteral(0).at(29..30),
                    BoolLiteral(true).at(32..36),
                ],
            }.at_boxed(27..37),
        }.at(0..37)
    )]
    #[case::lambda_taking_function_taking_function("(f: ((int) -> int) -> int) -> f((x: int) -> x)",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "f".to_owned(),
                    typ: Function {
                        parameter_types: vec![
                            Function {
                                parameter_types: vec![Integer.at(6..9)],
                                return_type: Integer.at_boxed(14..17),
                            }.at(5..17),
                        ],
                        return_type: Integer.at_boxed(22..25),
                    }.at(4..25),
                    span: (1..25).into(),
                },
            ],
            body: Call {
                callee: Reference("f".to_owned()).at_boxed(30..31),
                arguments: vec![
                    Lambda {
                        parameters: vec![
                            Parameter {
                                identifier: "x".to_owned(),
                                typ: Integer.at(36..39),
                                span: (33..39).into(),
                            }
                        ],
                        body: Reference("x".to_owned()).at_boxed(44..45),
                    }.at(32..45),
                ]
            }.at_boxed(30..46)
        }.at(0..46)
    )]
    #[case::lambda_taking_function_returning_function("(f: () -> () -> int) -> f()()",
        Lambda {
            parameters: vec![
                Parameter {
                    identifier: "f".to_owned(),
                    typ: Function {
                        parameter_types: vec![],
                        return_type: Function {
                            parameter_types: vec![],
                            return_type: Integer.at_boxed(16..19),
                        }.at_boxed(10..19),
                    }.at(4..19),
                    span: (1..19).into(),
                }
            ],
            body: Call {
                callee: Call {
                    callee: Reference("f".to_owned()).at_boxed(24..25),
                    arguments: vec![],
                }.at_boxed(24..27),
                arguments: vec![],
            }.at_boxed(24..29),
        }.at(0..29)
    )]
    fn parse_successful(#[case] code: &str, #[case] expected: Expression) {
        let tokens = lexer::lex(code).unwrap();
        let expression = parse(tokens);

        assert_that!(expression).contains_value(expected);
    }

    #[rstest]
    #[case(TokenKind::Then)]
    #[case(TokenKind::Else)]
    #[case(TokenKind::In)]
    #[case(TokenKind::Int)]
    #[case(TokenKind::Float)]
    #[case(TokenKind::Bool)]
    #[case(TokenKind::Addition)]
    #[case(TokenKind::Multiplication)]
    #[case(TokenKind::Division)]
    #[case(TokenKind::Modulo)]
    #[case(TokenKind::Equal)]
    #[case(TokenKind::NotEqual)]
    #[case(TokenKind::Less)]
    #[case(TokenKind::LessEqual)]
    #[case(TokenKind::Greater)]
    #[case(TokenKind::GreaterEqual)]
    #[case(TokenKind::Or)]
    #[case(TokenKind::Xor)]
    #[case(TokenKind::And)]
    #[case(TokenKind::Arrow)]
    #[case(TokenKind::Colon)]
    #[case(TokenKind::Comma)]
    #[case(TokenKind::Assign)]
    #[case(TokenKind::RightParenthesis)]
    fn parse_invalid_expression_starter(#[case] kind: TokenKind) {
        let token = kind.at(1..5);

        let result = parse(vec![token.clone()]);

        assert_that!(result).contains_error(ParseError::UnexpectedToken(token));
    }

    #[rstest]
    #[case::empty_code("", TokenKind::EndOfCode.at(0..0))]
    #[case::incomplete_binary("2 + ", TokenKind::EndOfCode.at(4..4))]
    #[case::incomplete_unary("!", TokenKind::EndOfCode.at(1..1))]
    #[case::missing_right_parenthesis("(1 + 2", TokenKind::EndOfCode.at(6..6))]
    #[case::colon_instead_of_right_parenthesis("(1 + 2:", TokenKind::Colon.at(6..7))]
    #[case::call_missing_closing_parentheses("f(x", TokenKind::EndOfCode.at(3..3))]
    #[case::call_missing_comma("f(x y)", TokenKind::Identifier("y".to_owned()).at(4..5))]
    #[case::if_missing_condition("if then 1 else 2", TokenKind::Then.at(3..7))]
    #[case::if_missing_then("if (true) x else y", TokenKind::Identifier("x".to_owned()).at(10..11))]
    #[case::if_missing_then_branch("if x then else 1", TokenKind::Else.at(10..14))]
    #[case::if_missing_else_branch("if x then y else", TokenKind::EndOfCode.at(16..16))]
    #[case::let_missing_in("let x = 3 x * x", TokenKind::Identifier("x".to_owned()).at(10..11))]
    #[case::let_missing_body("let x = 3 in", TokenKind::EndOfCode.at(12..12))]
    #[case::let_missing_assign("let x 3 in x", TokenKind::IntLiteral(3).at(6..7))]
    #[case::let_missing_assignment_value("let x = in x * x", TokenKind::In.at(8..10))]
    #[case::let_missing_comma_between_assignments(
        "let x = 3 y = 5 in x * y",
        TokenKind::Identifier("y".to_owned()).at(10..11)
    )]
    #[case::lambda_missing_closing_parenthesis("(a: int -> 1", TokenKind::Arrow.at(8..10))]
    #[case::lambda_missing_type("(x) -> 3", TokenKind::Arrow.at(4..6))]
    #[case::lambda_with_invalid_type(
        "(x: foo) -> x",
        TokenKind::Identifier("foo".to_owned()).at(4..7)
    )]
    #[case::lambda_missing_colon("(x int) -> x", TokenKind::Int.at(3..6))]
    #[case::lambda_with_trailing_comma("(x: int,) -> x", TokenKind::RightParenthesis.at(8..9))]
    #[case::lambda_missing_arrow("() 1", TokenKind::IntLiteral(1).at(3..4))]
    #[case::lambda_missing_body("() -> ", TokenKind::EndOfCode.at(6..6))]
    #[case::lambda_missing_type_of_second_parameter(
        "(x: int, y) -> x + y",
        TokenKind::RightParenthesis.at(10..11)
    )]
    #[case::function_type_missing_parentheses(
        "(f: int -> int) -> f(0)",
        TokenKind::Arrow.at(8..10)
    )]
    #[case::function_type_missing_return_type(
        "(f: (int) -> ) f(0)",
        TokenKind::RightParenthesis.at(13..14)
    )]
    #[case::function_type_with_trailing_comma(
        "(f: (int, float,) -> bool) -> f(1, 1.0)",
        TokenKind::RightParenthesis.at(16..17)
    )]
    fn parse_unexpected_token_at_later_point(#[case] code: &str, #[case] token: Token) {
        let tokens = lexer::lex(code).unwrap();
        let result = parse(tokens);

        assert_that!(result).contains_error(ParseError::UnexpectedToken(token));
    }

    #[rstest]
    #[case::after_expression("a + b")]
    #[case::during_lookahead("(x")]
    fn panics_on_missing_end_of_code(#[case] code: &str) {
        let mut tokens = lexer::lex(code).unwrap();
        tokens.pop(); // remove end-of-code

        assert_that!(move || parse(tokens)).panics_with_message(NO_EOC_MESSAGE);
    }

    #[test]
    fn panics_on_tokens_after_end_of_code() {
        let mut tokens = lexer::lex("x").unwrap();
        tokens.push(TokenKind::Addition.at(10..11));

        assert_that!(move || parse(tokens))
            .panics_with_message("found tokens after end-of-code token");
    }
}
