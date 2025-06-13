use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};

use crate::error::{ContextError, ContextResult, ExpectedType};
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::parser;
use crate::parser::TypeKind;
use crate::span::CodeSpan;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Function {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Function {
                parameter_types,
                return_type,
            } => {
                write!(f, "(")?;

                for (i, parameter_type) in parameter_types.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", parameter_type)?;
                }

                write!(f, ") -> ")?;
                write!(f, "{}", return_type)
            },
        }
    }
}

pub type DeclarationId = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub id: DeclarationId,
    pub value: Expression,
    pub span: CodeSpan,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Parameter {
    pub id: DeclarationId,
    pub typ: Type,
    pub span: CodeSpan,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Reference(DeclarationId),
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
    pub typ: Type,
    pub span: CodeSpan,
}

fn root_declarations() -> Vec<(&'static str, Type)> {
    vec![
        (
            "as_float",
            Type::Function {
                parameter_types: vec![Type::Integer],
                return_type: Box::new(Type::Float),
            },
        ),
        (
            "as_int",
            Type::Function {
                parameter_types: vec![Type::Float],
                return_type: Box::new(Type::Integer),
            },
        ),
        (
            "pow",
            Type::Function {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Box::new(Type::Float),
            },
        ),
        ("quality_level", Type::Integer),
        ("is_success", Type::Bool),
        ("is_critical_success", Type::Bool),
        ("is_spectacular_success", Type::Bool),
        ("is_failure", Type::Bool),
        ("is_critical_failure", Type::Bool),
        ("is_spectacular_failure", Type::Bool),
        ("remaining_fate_points", Type::Integer),
        (
            "remaining_aptitudes",
            Type::Function {
                parameter_types: vec![Type::Integer],
                return_type: Box::new(Type::Integer),
            },
        ),
        (
            "remaining_extra_skill_points",
            Type::Function {
                parameter_types: vec![Type::Integer],
                return_type: Box::new(Type::Integer),
            },
        ),
        (
            "remaining_extra_skill_points_on_success",
            Type::Function {
                parameter_types: vec![Type::Integer],
                return_type: Box::new(Type::Integer),
            },
        ),
        ("remaining_extra_quality_levels_on_success", Type::Integer),
    ]
}

struct DeclarationContext {
    types: HashMap<DeclarationId, Type>,
    frames: Vec<HashMap<String, DeclarationId>>,
    declaration_id_counter: DeclarationId,
}

impl DeclarationContext {
    fn new() -> DeclarationContext {
        let mut declaration_ctx = DeclarationContext {
            types: HashMap::new(),
            frames: vec![HashMap::new()],
            declaration_id_counter: 0,
        };

        for (identifier, typ) in root_declarations() {
            declaration_ctx.enter(identifier.to_owned(), typ);
        }

        declaration_ctx
    }

    fn get(&self, identifier: &str, span: CodeSpan) -> ContextResult<(DeclarationId, Type)> {
        let declaration_id = self
            .frames
            .iter()
            .rev()
            .filter_map(|frame| frame.get(identifier).copied())
            .next()
            .ok_or_else(|| ContextError::UnresolvedReference {
                identifier: identifier.to_owned(),
                span,
            })?;

        Ok((
            declaration_id,
            self.types.get(&declaration_id).unwrap().clone(),
        ))
    }

    fn enter(&mut self, identifier: String, typ: Type) -> DeclarationId {
        let declaration_id = self.declaration_id_counter;
        self.declaration_id_counter += 1;
        self.types.insert(declaration_id, typ);
        self.frames
            .last_mut()
            .unwrap()
            .insert(identifier, declaration_id);
        declaration_id
    }

    fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }
}

fn type_error<const LEN: usize>(
    expression: &Expression,
    expected_types: [Type; LEN],
) -> ContextError {
    ContextError::TypeError {
        actual_type: expression.typ.clone(),
        expected_types: expected_types.into_iter().map(ExpectedType::Type).collect(),
        span: expression.span,
    }
}

fn require_type<const LEN: usize>(
    expression: &Expression,
    expected_types: [Type; LEN],
) -> ContextResult<()> {
    let type_ok = expected_types
        .iter()
        .any(|expected_type| expected_type == &expression.typ);

    if type_ok {
        Ok(())
    }
    else {
        Err(type_error(expression, expected_types))
    }
}

fn map_type(typ: &parser::Type) -> Type {
    match &typ.kind {
        TypeKind::Integer => Type::Integer,
        TypeKind::Float => Type::Float,
        TypeKind::Bool => Type::Bool,
        TypeKind::Function {
            parameter_types,
            return_type,
        } => Type::Function {
            parameter_types: parameter_types.iter().map(map_type).collect(),
            return_type: Box::new(map_type(return_type)),
        },
    }
}

fn analyze_ctx(
    expression: parser::Expression,
    declaration_ctx: &mut DeclarationContext,
) -> ContextResult<Expression> {
    match expression.kind {
        parser::ExpressionKind::IntLiteral(value) => Ok(Expression {
            kind: ExpressionKind::IntLiteral(value),
            typ: Type::Integer,
            span: expression.span,
        }),
        parser::ExpressionKind::FloatLiteral(value) => Ok(Expression {
            kind: ExpressionKind::FloatLiteral(value),
            typ: Type::Float,
            span: expression.span,
        }),
        parser::ExpressionKind::BoolLiteral(value) => Ok(Expression {
            kind: ExpressionKind::BoolLiteral(value),
            typ: Type::Bool,
            span: expression.span,
        }),
        parser::ExpressionKind::Reference(identifier) => {
            let (declaration_id, typ) = declaration_ctx.get(&identifier, expression.span)?;

            Ok(Expression {
                kind: ExpressionKind::Reference(declaration_id),
                typ,
                span: expression.span,
            })
        },
        parser::ExpressionKind::UnaryOperation { operand, operator } => {
            let operand = analyze_ctx(*operand, declaration_ctx)?;

            match operator {
                UnaryOperator::Not => require_type(&operand, [Type::Bool])?,
                UnaryOperator::Negative => require_type(&operand, [Type::Integer, Type::Float])?,
            }

            Ok(Expression {
                typ: operand.typ.clone(),
                kind: ExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    operator,
                },
                span: expression.span,
            })
        },
        parser::ExpressionKind::BinaryOperation { lhs, rhs, operator } => {
            let lhs = analyze_ctx(*lhs, declaration_ctx)?;
            let rhs = analyze_ctx(*rhs, declaration_ctx)?;

            match operator {
                BinaryOperator::Addition
                | BinaryOperator::Subtraction
                | BinaryOperator::Multiplication
                | BinaryOperator::Division
                | BinaryOperator::Modulo
                | BinaryOperator::Less
                | BinaryOperator::LessEqual
                | BinaryOperator::Greater
                | BinaryOperator::GreaterEqual => {
                    require_type(&lhs, [Type::Integer, Type::Float])?;
                    require_type(&rhs, [lhs.typ.clone()])?;
                },
                BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => {
                    require_type(&lhs, [Type::Bool])?;
                    require_type(&rhs, [Type::Bool])?;
                },
                BinaryOperator::Equal | BinaryOperator::NotEqual => {
                    require_type(&lhs, [Type::Bool, Type::Integer, Type::Float])?;
                    require_type(&rhs, [lhs.typ.clone()])?;
                },
            }

            let result_type = match operator {
                BinaryOperator::Addition
                | BinaryOperator::Subtraction
                | BinaryOperator::Multiplication
                | BinaryOperator::Division
                | BinaryOperator::Modulo => lhs.typ.clone(),
                BinaryOperator::Less
                | BinaryOperator::LessEqual
                | BinaryOperator::Greater
                | BinaryOperator::GreaterEqual
                | BinaryOperator::And
                | BinaryOperator::Or
                | BinaryOperator::Xor
                | BinaryOperator::Equal
                | BinaryOperator::NotEqual => Type::Bool,
            };

            Ok(Expression {
                typ: result_type,
                kind: ExpressionKind::BinaryOperation {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    operator,
                },
                span: expression.span,
            })
        },
        parser::ExpressionKind::Call { callee, arguments } => {
            let callee = analyze_ctx(*callee, declaration_ctx)?;
            let (parameter_types, return_type) = match &callee.typ {
                Type::Function {
                    parameter_types,
                    return_type,
                } => (parameter_types, return_type.as_ref()),
                _ => {
                    return Err(ContextError::TypeError {
                        actual_type: callee.typ,
                        expected_types: vec![ExpectedType::AnyFunction],
                        span: callee.span,
                    });
                },
            };

            if parameter_types.len() != arguments.len() {
                return Err(ContextError::CardinalityError {
                    callee_type: callee.typ.clone(),
                    parameter_count: parameter_types.len(),
                    argument_count: arguments.len(),
                    span: expression.span,
                });
            }

            let arguments = parameter_types
                .iter()
                .zip(arguments.into_iter())
                .map(|(parameter_type, argument)| {
                    analyze_ctx(argument, declaration_ctx).and_then(|argument| {
                        require_type(&argument, [parameter_type.clone()])?;
                        Ok(argument)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Expression {
                typ: return_type.clone(),
                kind: ExpressionKind::Call {
                    callee: Box::new(callee),
                    arguments,
                },
                span: expression.span,
            })
        },
        parser::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = analyze_ctx(*condition, declaration_ctx)?;
            let then_branch = analyze_ctx(*then_branch, declaration_ctx)?;
            let else_branch = analyze_ctx(*else_branch, declaration_ctx)?;
            let typ = then_branch.typ.clone();

            require_type(&condition, [Type::Bool])?;
            require_type(&else_branch, [typ.clone()])?;

            Ok(Expression {
                typ,
                kind: ExpressionKind::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                span: expression.span,
            })
        },
        parser::ExpressionKind::Let { assignments, body } => {
            declaration_ctx.push_frame();

            let assignments = assignments
                .into_iter()
                .map(|assignment| {
                    let value = analyze_ctx(assignment.value, declaration_ctx)?;
                    let id = declaration_ctx.enter(assignment.identifier, value.typ.clone());

                    Ok(Assignment {
                        id,
                        value,
                        span: assignment.span,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = analyze_ctx(*body, declaration_ctx)?;

            declaration_ctx.pop_frame();

            Ok(Expression {
                typ: body.typ.clone(),
                kind: ExpressionKind::Let {
                    assignments,
                    body: Box::new(body),
                },
                span: expression.span,
            })
        },
        parser::ExpressionKind::Lambda { parameters, body } => {
            declaration_ctx.push_frame();

            let parameters = parameters
                .into_iter()
                .map(|parameter| {
                    let typ = map_type(&parameter.typ);
                    let id = declaration_ctx.enter(parameter.identifier, typ.clone());

                    Ok(Parameter {
                        id,
                        typ,
                        span: parameter.span,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = analyze_ctx(*body, declaration_ctx)?;

            declaration_ctx.pop_frame();

            Ok(Expression {
                typ: Type::Function {
                    parameter_types: parameters
                        .iter()
                        .map(|parameter| parameter.typ.clone())
                        .collect(),
                    return_type: Box::new(body.typ.clone()),
                },
                kind: ExpressionKind::Lambda {
                    parameters,
                    body: Box::new(body),
                },
                span: expression.span,
            })
        },
    }
}

/// Performs contextual analysis on a parsed Scholle expression. The returned expression contains
/// information about references (identifiers are substituted for declaration-IDs) and types. It is
/// also checked that all references can be resolved and the types are valid. In case of a
/// violation, an appropriate [ContextError] is returned.
pub fn analyze(expression: parser::Expression) -> ContextResult<Expression> {
    let mut declaration_ctx = DeclarationContext::new();
    let expression = analyze_ctx(expression, &mut declaration_ctx)?;

    require_type(&expression, [Type::Float, Type::Integer])?;

    Ok(expression)
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use kernal::prelude::*;
    use rstest::rstest;

    use super::ExpressionKind::*;
    use super::*;
    use crate::lexer;

    impl ExpressionKind {
        fn to_expr(self, typ: Type, span_range: Range<usize>) -> Expression {
            Expression {
                kind: self,
                typ,
                span: span_range.into(),
            }
        }

        fn to_boxed_expr(self, typ: Type, span_range: Range<usize>) -> Box<Expression> {
            Box::new(self.to_expr(typ, span_range))
        }
    }

    fn new_decl_id(offset: DeclarationId) -> DeclarationId {
        root_declarations().len() + offset
    }

    fn builtin_decl_id(identifier: &str) -> DeclarationId {
        root_declarations()
            .into_iter()
            .enumerate()
            .find(|(_, (builtin_identifier, _))| builtin_identifier == &identifier)
            .map(|(index, _)| index)
            .unwrap()
    }

    fn int_to_int_function() -> Type {
        Type::Function {
            parameter_types: vec![Type::Integer],
            return_type: Box::new(Type::Integer),
        }
    }

    fn parse_and_analyze(code: &str) -> ContextResult<Expression> {
        let tokens = lexer::lex(code).unwrap();
        let ast = parser::parse(tokens).unwrap();
        analyze(ast)
    }

    #[rstest]
    #[case::int_literal("123", IntLiteral(123).to_expr(Type::Integer, 0..3))]
    #[case::float_literal("123.456", FloatLiteral(123.456).to_expr(Type::Float, 0..7))]
    #[case::simple_let_binding("let x = 1 in x + 1",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(0),
                    value: IntLiteral(1).to_expr(Type::Integer, 8..9),
                    span: (4..9).into(),
                }
            ],
            body: BinaryOperation {
                lhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Integer, 13..14),
                rhs: IntLiteral(1).to_boxed_expr(Type::Integer, 17..18),
                operator: BinaryOperator::Addition,
            }.to_boxed_expr(Type::Integer, 13..18),
        }.to_expr(Type::Integer, 0..18)
    )]
    #[case::chained_let_binding("let x = 1.5, y = x * x in y * y",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(0),
                    value: FloatLiteral(1.5).to_expr(Type::Float, 8..11),
                    span: (4..11).into(),
                },
                Assignment {
                    id: new_decl_id(1),
                    value: BinaryOperation {
                        lhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Float, 17..18),
                        rhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Float, 21..22),
                        operator: BinaryOperator::Multiplication,
                    }.to_expr(Type::Float, 17..22),
                    span: (13..22).into(),
                }
            ],
            body: BinaryOperation {
                lhs: Reference(new_decl_id(1)).to_boxed_expr(Type::Float, 26..27),
                rhs: Reference(new_decl_id(1)).to_boxed_expr(Type::Float, 30..31),
                operator: BinaryOperator::Multiplication,
            }.to_boxed_expr(Type::Float, 26..31),
        }.to_expr(Type::Float, 0..31)
    )]
    #[case::simple_function("let sq = (x: float) -> x * x in sq(3.0)",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(1),
                    value: Lambda {
                        parameters: vec![
                            Parameter {
                                id: new_decl_id(0),
                                typ: Type::Float,
                                span: (10..18).into(),
                            }
                        ],
                        body: BinaryOperation {
                            lhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Float, 23..24),
                            rhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Float, 27..28),
                            operator: BinaryOperator::Multiplication,
                        }.to_boxed_expr(Type::Float, 23..28),
                    }.to_expr(Type::Function {
                        parameter_types: vec![Type::Float],
                        return_type: Box::new(Type::Float),
                    }, 9..28),
                    span: (4..28).into()
                }
            ],
            body: Call {
                callee: Reference(new_decl_id(1))
                    .to_boxed_expr(Type::Function {
                        parameter_types: vec![Type::Float],
                        return_type: Box::new(Type::Float),
                    }, 32..34),
                arguments: vec![
                    FloatLiteral(3.0).to_expr(Type::Float, 35..38)
                ],
            }.to_boxed_expr(Type::Float, 32..39)
        }.to_expr(Type::Float, 0..39)
    )]
    #[case::higher_order_function(
        "let with_1 = (fun: (int) -> int) -> () -> fun(1) in with_1((x: int) -> x)()",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(1),
                    value: Lambda {
                        parameters: vec![
                            Parameter {
                                id: new_decl_id(0),
                                typ: int_to_int_function(),
                                span: (14..31).into(),
                            }
                        ],
                        body: Lambda {
                            parameters: vec![],
                            body: Call {
                                callee: Reference(new_decl_id(0))
                                    .to_boxed_expr(int_to_int_function(), 42..45),
                                arguments: vec![
                                    IntLiteral(1).to_expr(Type::Integer, 46..47),
                                ],
                            }.to_boxed_expr(Type::Integer, 42..48),
                        }.to_boxed_expr(Type::Function {
                            parameter_types: vec![],
                            return_type: Box::new(Type::Integer),
                        }, 36..48)
                    }.to_expr(Type::Function {
                        parameter_types: vec![int_to_int_function()],
                        return_type: Box::new(Type::Function {
                            parameter_types: vec![],
                            return_type: Box::new(Type::Integer),
                        })
                    }, 13..48),
                    span: (4..48).into(),
                }
            ],
            body: Call {
                callee: Call {
                    callee: Reference(new_decl_id(1))
                        .to_boxed_expr(Type::Function {
                            parameter_types: vec![int_to_int_function()],
                            return_type: Box::new(Type::Function {
                                parameter_types: vec![],
                                return_type: Box::new(Type::Integer),
                            })
                        }, 52..58),
                    arguments: vec![
                        Lambda {
                            parameters: vec![
                                Parameter {
                                    id: new_decl_id(2),
                                    typ: Type::Integer,
                                    span: (60..66).into(),
                                }
                            ],
                            body: Reference(new_decl_id(2)).to_boxed_expr(Type::Integer, 71..72),
                        }.to_expr(int_to_int_function(), 59..72),
                    ],
                }.to_boxed_expr(Type::Function {
                    parameter_types: vec![],
                    return_type: Box::new(Type::Integer),
                }, 52..73),
                arguments: vec![],
            }.to_boxed_expr(Type::Integer, 52..75),
        }.to_expr(Type::Integer, 0..75)
    )]
    #[case::shadowing_let_in_let("let x = 1, y = let x = 2 in x in x + y",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(0),
                    value: IntLiteral(1).to_expr(Type::Integer, 8..9),
                    span: (4..9).into(),
                },
                Assignment {
                    id: new_decl_id(2),
                    value: Let {
                        assignments: vec![
                            Assignment {
                                id: new_decl_id(1),
                                value: IntLiteral(2).to_expr(Type::Integer, 23..24),
                                span: (19..24).into(),
                            }
                        ],
                        body: Reference(new_decl_id(1)).to_boxed_expr(Type::Integer, 28..29),
                    }.to_expr(Type::Integer, 15..29),
                    span: (11..29).into()
                },
            ],
            body: BinaryOperation {
                lhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Integer, 33..34),
                rhs: Reference(new_decl_id(2)).to_boxed_expr(Type::Integer, 37..38),
                operator: BinaryOperator::Addition,
            }.to_boxed_expr(Type::Integer, 33..38),
        }.to_expr(Type::Integer, 0..38)
    )]
    #[case::shadowing_function_in_let("let x = 2.0, f = (x: float) -> x in f(x)",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(0),
                    value: FloatLiteral(2.0).to_expr(Type::Float, 8..11),
                    span: (4..11).into(),
                },
                Assignment {
                    id: new_decl_id(2),
                    value: Lambda {
                        parameters: vec![
                            Parameter {
                                id: new_decl_id(1),
                                typ: Type::Float,
                                span: (18..26).into(),
                            }
                        ],
                        body: Reference(new_decl_id(1)).to_boxed_expr(Type::Float, 31..32),
                    }.to_expr(Type::Function {
                        parameter_types: vec![Type::Float],
                        return_type: Box::new(Type::Float),
                    }, 17..32),
                    span: (13..32).into(),
                },
            ],
            body: Call {
                callee: Reference(new_decl_id(2)).to_boxed_expr(Type::Function {
                    parameter_types: vec![Type::Float],
                    return_type: Box::new(Type::Float),
                }, 36..37),
                arguments: vec![
                    Reference(new_decl_id(0)).to_expr(Type::Float, 38..39),
                ],
            }.to_boxed_expr(Type::Float, 36..40),
        }.to_expr(Type::Float, 0..40)
    )]
    #[case::shadowing_let_in_function("((x: int) -> let x = x + 1 in x)(1)",
        Call {
            callee: Lambda {
                parameters: vec![
                    Parameter {
                        id: new_decl_id(0),
                        typ: Type::Integer,
                        span: (2..8).into(),
                    }
                ],
                body: Let {
                    assignments: vec![
                        Assignment {
                            id: new_decl_id(1),
                            value: BinaryOperation {
                                lhs: Reference(new_decl_id(0)).to_boxed_expr(Type::Integer, 21..22),
                                rhs: IntLiteral(1).to_boxed_expr(Type::Integer, 25..26),
                                operator: BinaryOperator::Addition,
                            }.to_expr(Type::Integer, 21..26),
                            span: (17..26).into(),
                        },
                    ],
                    body: Reference(new_decl_id(1)).to_boxed_expr(Type::Integer, 30..31),
                }.to_boxed_expr(Type::Integer, 13..31),
            }.to_boxed_expr(int_to_int_function(), 0..32),
            arguments: vec![
                IntLiteral(1).to_expr(Type::Integer, 33..34),
            ],
        }.to_expr(Type::Integer, 0..35)
    )]
    #[case::shadowing_function_in_function("((x: int) -> (x: int) -> x)(1)(1)",
        Call {
            callee: Call {
                callee: Lambda {
                    parameters: vec![
                        Parameter {
                            id: new_decl_id(0),
                            typ: Type::Integer,
                            span: (2..8).into(),
                        }
                    ],
                    body: Lambda {
                        parameters: vec![
                            Parameter {
                                id: new_decl_id(1),
                                typ: Type::Integer,
                                span: (14..20).into(),
                            }
                        ],
                        body: Reference(new_decl_id(1)).to_boxed_expr(Type::Integer, 25..26),
                    }.to_boxed_expr(int_to_int_function(), 13..26),
                }.to_boxed_expr(Type::Function {
                    parameter_types: vec![Type::Integer],
                    return_type: Box::new(int_to_int_function()),
                }, 0..27),
                arguments: vec![
                    IntLiteral(1).to_expr(Type::Integer, 28..29),
                ]
            }.to_boxed_expr(int_to_int_function(), 0..30),
            arguments: vec![
                IntLiteral(1).to_expr(Type::Integer, 31..32),
            ]
        }.to_expr(Type::Integer, 0..33)
    )]
    #[case::simple_if("if true then 1 else 2",
        If {
            condition: BoolLiteral(true).to_boxed_expr(Type::Bool, 3..7),
            then_branch: IntLiteral(1).to_boxed_expr(Type::Integer, 13..14),
            else_branch: IntLiteral(2).to_boxed_expr(Type::Integer, 20..21),
        }.to_expr(Type::Integer, 0..21)
    )]
    #[case::complex_if_condition("if 1 > 2 | 1.1 <= 1.0 then 1.0 else 0.5",
        If {
            condition: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: IntLiteral(1).to_boxed_expr(Type::Integer, 3..4),
                    rhs: IntLiteral(2).to_boxed_expr(Type::Integer, 7..8),
                    operator: BinaryOperator::Greater,
                }.to_boxed_expr(Type::Bool, 3..8),
                rhs: BinaryOperation {
                    lhs: FloatLiteral(1.1).to_boxed_expr(Type::Float, 11..14),
                    rhs: FloatLiteral(1.0).to_boxed_expr(Type::Float, 18..21),
                    operator: BinaryOperator::LessEqual,
                }.to_boxed_expr(Type::Bool, 11..21),
                operator: BinaryOperator::Or,
            }.to_boxed_expr(Type::Bool, 3..21),
            then_branch: FloatLiteral(1.0).to_boxed_expr(Type::Float, 27..30),
            else_branch: FloatLiteral(0.5).to_boxed_expr(Type::Float, 36..39),
        }.to_expr(Type::Float, 0..39)
    )]
    #[case::nested_if(
        "if 1 < 2 then if !(true & false) then 1 else 0 else if 1 >= 2 then 3 else 2",
        If {
            condition: BinaryOperation {
                lhs: IntLiteral(1).to_boxed_expr(Type::Integer, 3..4),
                rhs: IntLiteral(2).to_boxed_expr(Type::Integer, 7..8),
                operator: BinaryOperator::Less,
            }.to_boxed_expr(Type::Bool, 3..8),
            then_branch: If {
                condition: UnaryOperation {
                    operand: BinaryOperation {
                        lhs: BoolLiteral(true).to_boxed_expr(Type::Bool, 19..23),
                        rhs: BoolLiteral(false).to_boxed_expr(Type::Bool, 26..31),
                        operator: BinaryOperator::And,
                    }.to_boxed_expr(Type::Bool, 18..32),
                    operator: UnaryOperator::Not,
                }.to_boxed_expr(Type::Bool, 17..32),
                then_branch: IntLiteral(1).to_boxed_expr(Type::Integer, 38..39),
                else_branch: IntLiteral(0).to_boxed_expr(Type::Integer, 45..46),
            }.to_boxed_expr(Type::Integer, 14..46),
            else_branch: If {
                condition: BinaryOperation {
                    lhs: IntLiteral(1).to_boxed_expr(Type::Integer, 55..56),
                    rhs: IntLiteral(2).to_boxed_expr(Type::Integer, 60..61),
                    operator: BinaryOperator::GreaterEqual,
                }.to_boxed_expr(Type::Bool, 55..61),
                then_branch: IntLiteral(3).to_boxed_expr(Type::Integer, 67..68),
                else_branch: IntLiteral(2).to_boxed_expr(Type::Integer, 74..75),
            }.to_boxed_expr(Type::Integer, 52..75),
        }.to_expr(Type::Integer, 0..75)
    )]
    #[case::function_taking_bool("let f = (b: bool) -> if b then 1 else 0 in f(true)",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(1),
                    value: Lambda {
                        parameters: vec![
                            Parameter {
                                id: new_decl_id(0),
                                typ: Type::Bool,
                                span: (9..16).into(),
                            }
                        ],
                        body: If {
                            condition: Reference(new_decl_id(0)).to_boxed_expr(Type::Bool, 24..25),
                            then_branch: IntLiteral(1).to_boxed_expr(Type::Integer, 31..32),
                            else_branch: IntLiteral(0).to_boxed_expr(Type::Integer, 38..39),
                        }.to_boxed_expr(Type::Integer, 21..39),
                    }.to_expr(Type::Function {
                        parameter_types: vec![Type::Bool],
                        return_type: Box::new(Type::Integer),
                    }, 8..39),
                    span: (4..39).into(),
                }
            ],
            body: Call {
                callee: Reference(new_decl_id(1)).to_boxed_expr(Type::Function {
                    parameter_types: vec![Type::Bool],
                    return_type: Box::new(Type::Integer),
                }, 43..44),
                arguments: vec![
                    BoolLiteral(true).to_expr(Type::Bool, 45..49),
                ],
            }.to_boxed_expr(Type::Integer, 43..50),
        }.to_expr(Type::Integer, 0..50)
    )]
    #[case::remaining_operators("if 1 == 2 ^ true != false then -1.0 / 2.0 else 5.0 % 3.0",
        If {
            condition: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: IntLiteral(1).to_boxed_expr(Type::Integer, 3..4),
                    rhs: IntLiteral(2).to_boxed_expr(Type::Integer, 8..9),
                    operator: BinaryOperator::Equal,
                }.to_boxed_expr(Type::Bool, 3..9),
                rhs: BinaryOperation {
                    lhs: BoolLiteral(true).to_boxed_expr(Type::Bool, 12..16),
                    rhs: BoolLiteral(false).to_boxed_expr(Type::Bool, 20..25),
                    operator: BinaryOperator::NotEqual,
                }.to_boxed_expr(Type::Bool, 12..25),
                operator: BinaryOperator::Xor,
            }.to_boxed_expr(Type::Bool, 3..25),
            then_branch: BinaryOperation {
                lhs: UnaryOperation {
                    operand: FloatLiteral(1.0).to_boxed_expr(Type::Float, 32..35),
                    operator: UnaryOperator::Negative,
                }.to_boxed_expr(Type::Float, 31..35),
                rhs: FloatLiteral(2.0).to_boxed_expr(Type::Float, 38..41),
                operator: BinaryOperator::Division,
            }.to_boxed_expr(Type::Float, 31..41),
            else_branch: BinaryOperation {
                lhs: FloatLiteral(5.0).to_boxed_expr(Type::Float, 47..50),
                rhs: FloatLiteral(3.0).to_boxed_expr(Type::Float, 53..56),
                operator: BinaryOperator::Modulo,
            }.to_boxed_expr(Type::Float, 47..56),
        }.to_expr(Type::Float, 0..56)
    )]
    #[case::builtins_1("pow(2.0, as_float(quality_level))",
        Call {
            callee: Reference(builtin_decl_id("pow")).to_boxed_expr(Type::Function {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Box::new(Type::Float),
            }, 0..3),
            arguments: vec![
                FloatLiteral(2.0).to_expr(Type::Float, 4..7),
                Call {
                    callee: Reference(builtin_decl_id("as_float")).to_boxed_expr(Type::Function {
                        parameter_types: vec![Type::Integer],
                        return_type: Box::new(Type::Float),
                    }, 9..17),
                    arguments: vec![
                        Reference(builtin_decl_id("quality_level")).to_expr(Type::Integer, 18..31),
                    ],
                }.to_expr(Type::Float, 9..32),
            ],
        }.to_expr(Type::Float, 0..33)
    )]
    #[case::builtins_2(
        "if is_success | is_critical_success | is_spectacular_success \
            then remaining_fate_points else remaining_extra_quality_levels_on_success",
        If {
            condition: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference(builtin_decl_id("is_success")).to_boxed_expr(Type::Bool, 3..13),
                    rhs: Reference(builtin_decl_id("is_critical_success"))
                        .to_boxed_expr(Type::Bool, 16..35),
                    operator: BinaryOperator::Or,
                }.to_boxed_expr(Type::Bool, 3..35),
                rhs: Reference(builtin_decl_id("is_spectacular_success"))
                    .to_boxed_expr(Type::Bool, 38..60),
                operator: BinaryOperator::Or,
            }.to_boxed_expr(Type::Bool, 3..60),
            then_branch: Reference(builtin_decl_id("remaining_fate_points"))
                .to_boxed_expr(Type::Integer, 66..87),
            else_branch: Reference(builtin_decl_id("remaining_extra_quality_levels_on_success"))
                .to_boxed_expr(Type::Integer, 93..134),
        }.to_expr(Type::Integer, 0..134)
    )]
    #[case::builtins_3(
        "if is_failure & is_critical_failure & is_spectacular_failure then 0 else as_int(1.0)",
        If {
            condition: BinaryOperation {
                lhs: BinaryOperation {
                    lhs: Reference(builtin_decl_id("is_failure")).to_boxed_expr(Type::Bool, 3..13),
                    rhs: Reference(builtin_decl_id("is_critical_failure"))
                        .to_boxed_expr(Type::Bool, 16..35),
                    operator: BinaryOperator::And,
                }.to_boxed_expr(Type::Bool, 3..35),
                rhs: Reference(builtin_decl_id("is_spectacular_failure"))
                    .to_boxed_expr(Type::Bool, 38..60),
                operator: BinaryOperator::And,
            }.to_boxed_expr(Type::Bool, 3..60),
            then_branch: IntLiteral(0).to_boxed_expr(Type::Integer, 66..67),
            else_branch: Call {
                callee: Reference(builtin_decl_id("as_int")).to_boxed_expr(Type::Function {
                    parameter_types: vec![Type::Float],
                    return_type: Box::new(Type::Integer),
                }, 73..79),
                arguments: vec![
                    FloatLiteral(1.0).to_expr(Type::Float, 80..83),
                ],
            }.to_boxed_expr(Type::Integer, 73..84),
        }.to_expr(Type::Integer, 0..84)
    )]
    #[case::builtins_4(
        "remaining_aptitudes(1) + remaining_extra_skill_points(2) + \
            remaining_extra_skill_points_on_success(3)",
        BinaryOperation {
            lhs: BinaryOperation {
                lhs: Call {
                    callee: Reference(builtin_decl_id("remaining_aptitudes"))
                        .to_boxed_expr(int_to_int_function(), 0..19),
                    arguments: vec![
                        IntLiteral(1).to_expr(Type::Integer, 20..21),
                    ]
                }.to_boxed_expr(Type::Integer, 0..22),
                rhs: Call {
                    callee: Reference(builtin_decl_id("remaining_extra_skill_points"))
                        .to_boxed_expr(int_to_int_function(), 25..53),
                    arguments: vec![
                        IntLiteral(2).to_expr(Type::Integer, 54..55),
                    ],
                }.to_boxed_expr(Type::Integer, 25..56),
                operator: BinaryOperator::Addition,
            }.to_boxed_expr(Type::Integer, 0..56),
            rhs: Call {
                callee: Reference(builtin_decl_id("remaining_extra_skill_points_on_success"))
                    .to_boxed_expr(int_to_int_function(), 59..98),
                arguments: vec![
                    IntLiteral(3).to_expr(Type::Integer, 99..100),
                ],
            }.to_boxed_expr(Type::Integer, 59..101),
            operator: BinaryOperator::Addition,
        }.to_expr(Type::Integer, 0..101)
    )]
    #[case::shadowing_builtin("let quality_level = quality_level - 1 in quality_level",
        Let {
            assignments: vec![
                Assignment {
                    id: new_decl_id(0),
                    value: BinaryOperation {
                        lhs: Reference(builtin_decl_id("quality_level"))
                            .to_boxed_expr(Type::Integer, 20..33),
                        rhs: IntLiteral(1).to_boxed_expr(Type::Integer, 36..37),
                        operator: BinaryOperator::Subtraction,
                    }.to_expr(Type::Integer, 20..37),
                    span: (4..37).into(),
                }
            ],
            body: Reference(new_decl_id(0)).to_boxed_expr(Type::Integer, 41..54),
        }.to_expr(Type::Integer, 0..54)
    )]
    fn analyze_success(#[case] code: &str, #[case] expected_result: Expression) {
        let result = parse_and_analyze(code);

        assert_that!(result).contains_value(expected_result);
    }

    #[rstest]
    #[case::direct("x", "x", 0..1)]
    #[case::before_declaration("let x = y, y = 1 in x", "y", 8..9)]
    #[case::out_of_scope_parameter("let fn = (x: float) -> x, y = x in 0.0", "x", 30..31)]
    #[case::out_of_scope_assignment("(let value = 1 in value) + value", "value", 27..32)]
    #[case::no_recursion("let f = (x: int) -> if x == 0 then 1 else f(x - 1) in f(5)", "f", 42..43)]
    fn analyze_unresolved_reference(
        #[case] code: &str,
        #[case] identifier: &str,
        #[case] span: impl Into<CodeSpan>,
    ) {
        let result = parse_and_analyze(code);

        assert_that!(result).contains_error(ContextError::UnresolvedReference {
            identifier: identifier.to_owned(),
            span: span.into(),
        });
    }

    #[rstest]
    #[case::top_level_bool("true", Type::Bool, [Type::Float, Type::Integer], 0..4)]
    #[case::top_level_function(
        "(x: int) -> x",
        int_to_int_function(),
        [Type::Float, Type::Integer],
        0..13,
    )]
    #[case::not_with_int("!1", Type::Integer, [Type::Bool], 1..2)]
    #[case::negative_with_bool("-true", Type::Bool, [Type::Float, Type::Integer], 1..5)]
    #[case::arithmetic_lhs_function(
        "((x: int) -> x) * 3.0",
        int_to_int_function(),
        [Type::Float, Type::Integer],
        0..15,
    )]
    #[case::arithmetic_rhs_bool("1 + true", Type::Bool, [Type::Integer], 4..8)]
    #[case::comparison_lhs_bool(
        "if true > 1 then 1 else 0",
        Type::Bool,
        [Type::Integer, Type::Float],
        3..7,
    )]
    #[case::comparison_rhs_function(
        "if 2.0 <= (x: float) -> x then 5.0 else 4.0",
        Type::Function { parameter_types: vec![Type::Float], return_type: Box::new(Type::Float) },
        [Type::Float],
        10..25,
    )]
    #[case::logical_lhs_int("1 & true", Type::Integer, [Type::Bool], 0..1)]
    #[case::logical_rhs_float("false | 0.5", Type::Float, [Type::Bool], 8..11)]
    #[case::logical_lhs_function(
        "((x: int) -> x) ^ is_success",
        int_to_int_function(),
        [Type::Bool],
        0..15,
    )]
    #[case::equality_lhs_function(
        "((x: int) -> x) == ((y: int) -> y)",
        int_to_int_function(),
        [Type::Float, Type::Integer, Type::Bool],
        0..15,
    )]
    #[case::equality_rhs_function(
        "5 != (x: int) -> x",
        int_to_int_function(),
        [Type::Integer],
        5..18,
    )]
    #[case::if_condition_int("if quality_level then 1 else 0", Type::Integer, [Type::Bool], 3..16)]
    #[case::if_condition_float("if 1.0 then 1.0 else 0.0", Type::Float, [Type::Bool], 3..6)]
    #[case::if_condition_function(
        "if (x: int) -> x then 1.0 else 0.0",
        int_to_int_function(),
        [Type::Bool],
        3..16,
    )]
    #[case::if_else_does_not_match_then(
        "if is_success then 1.0 else 0",
        Type::Integer,
        [Type::Float],
        28..29,
    )]
    #[case::call_invalid_argument_type("pow(1.0, 2)", Type::Integer, [Type::Float], 9..10)]
    #[case::callee_is_float("let x = 1.0 in x()", Type::Float, [ExpectedType::AnyFunction], 15..16)]
    #[case::callee_is_int("let x = 1 in x()", Type::Integer, [ExpectedType::AnyFunction], 13..14)]
    #[case::callee_is_bool("let x = true in x()", Type::Bool, [ExpectedType::AnyFunction], 16..17)]
    fn analyze_type_error<ExpectedTypeT>(
        #[case] code: &str,
        #[case] expected_actual_type: Type,
        #[case] expected_expected_types: ExpectedTypeT,
        #[case] expected_span: impl Into<CodeSpan>,
    ) where
        ExpectedTypeT: IntoIterator,
        ExpectedTypeT::Item: Into<ExpectedType>,
    {
        let err = parse_and_analyze(code).unwrap_err();

        if let ContextError::TypeError {
            actual_type,
            expected_types,
            span,
        } = err
        {
            let expected_expected_types = expected_expected_types
                .into_iter()
                .map(Into::into)
                .collect::<Vec<_>>();

            assert_that!(actual_type).is_equal_to(expected_actual_type);
            assert_that!(expected_types).contains_exactly_in_any_order(expected_expected_types);
            assert_that!(span).is_equal_to(expected_span.into());
        }
    }

    #[rstest]
    #[case::empty_function_with_argument("let f = () -> 1 in f(1)", [], Type::Integer, 1, 19..23)]
    #[case::function_called_with_too_many_arguments(
        "let f = (x: float) -> x * 2.0 in f(1.0, true)",
        [Type::Float],
        Type::Float,
        2,
        33..45,
    )]
    #[case::function_called_with_too_few_arguments(
        "let f = (x: float, b: bool) -> if b then x else 0.0 in f(1.0)",
        [Type::Float, Type::Bool],
        Type::Float,
        1,
        55..61,
    )]
    #[case::function_called_with_no_arguments(
        "let fma = (x: float, y: float, z: float) -> x + y * z in fma()",
        [Type::Float, Type::Float, Type::Float],
        Type::Float,
        0,
        57..62,
    )]
    fn analyze_cardinality_error(
        #[case] code: &str,
        #[case] parameter_types: impl IntoIterator<Item = Type>,
        #[case] return_type: Type,
        #[case] argument_count: usize,
        #[case] span: impl Into<CodeSpan>,
    ) {
        let result = parse_and_analyze(code);

        let parameter_types = parameter_types.into_iter().collect::<Vec<_>>();
        let parameter_count = parameter_types.len();

        assert_that!(result).contains_error(ContextError::CardinalityError {
            callee_type: Type::Function {
                parameter_types,
                return_type: Box::new(return_type),
            },
            parameter_count,
            argument_count,
            span: span.into(),
        });
    }
}
