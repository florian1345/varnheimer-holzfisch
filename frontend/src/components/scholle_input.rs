use dioxus::prelude::*;
use scholle::context::Type;
use scholle::error::{
    ContextError,
    ExpectedType,
    LexerError,
    LexerErrorKind,
    ParseError,
    RuntimeError,
    RuntimeErrorKind,
    ScholleError,
};
use scholle::lexer::TokenKind;
use scholle::span::CodeSpan;
use scholle::{ScholleEvaluator, lexer};

use crate::DEFAULT_SCHOLLE_CODE;
use crate::display::EnumerationDisplay;

const SCHOLLE_DOCUMENTATION_LINK: &str =
    "https://github.com/florian1345/varnheimer-holzfisch/blob/master/scholle/README.md";

#[derive(Clone, Copy, Eq, PartialEq)]
enum Highlighting {
    Ordinary,
    Keyword,
    Number,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct ScholleCodeSegment<'code> {
    segment: &'code str,
    highlighting: Highlighting,
    error: bool,
}

impl<'code> ScholleCodeSegment<'code> {
    fn class(&self) -> &'static str {
        match (self.highlighting, self.error) {
            (Highlighting::Ordinary, false) => "scholle-ordinary",
            (Highlighting::Keyword, false) => "scholle-keyword",
            (Highlighting::Number, false) => "scholle-number",
            (Highlighting::Ordinary, true) => "scholle-ordinary scholle-error",
            (Highlighting::Keyword, true) => "scholle-keyword scholle-error",
            (Highlighting::Number, true) => "scholle-number scholle-error",
        }
    }
}

fn highlighting_for_token(kind: &TokenKind) -> Highlighting {
    match kind {
        TokenKind::True
        | TokenKind::False
        | TokenKind::If
        | TokenKind::Then
        | TokenKind::Else
        | TokenKind::Let
        | TokenKind::In
        | TokenKind::Int
        | TokenKind::Float
        | TokenKind::Bool => Highlighting::Keyword,
        TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) => Highlighting::Number,
        _ => Highlighting::Ordinary,
    }
}

fn split_on_error(code: &str, error_span: CodeSpan) -> Vec<ScholleCodeSegment> {
    let mut segments = Vec::new();

    if error_span.start_byte > 0 {
        segments.push(ScholleCodeSegment {
            segment: &code[..error_span.start_byte],
            highlighting: Highlighting::Ordinary,
            error: false,
        });
    }

    segments.push(ScholleCodeSegment {
        segment: &code[error_span.start_byte..error_span.end_byte],
        highlighting: Highlighting::Ordinary,
        error: true,
    });

    if error_span.end_byte < code.len() {
        segments.push(ScholleCodeSegment {
            segment: &code[error_span.end_byte..],
            highlighting: Highlighting::Ordinary,
            error: false,
        });
    }

    segments
}

fn to_segments<'code>(
    code: &'code str,
    error: Option<&ScholleError>,
) -> Vec<ScholleCodeSegment<'code>> {
    let tokens = match lexer::lex(code) {
        Ok(tokens) => tokens,
        Err(e) => return split_on_error(code, e.span),
    };

    let error_span = error
        .map(ScholleError::span)
        .unwrap_or(CodeSpan::from(usize::MAX..usize::MAX));
    let mut segments = vec![];
    let mut current_segment_highlighting = highlighting_for_token(&tokens[0].kind);
    let mut current_error = error_span.start_byte == 0;
    let mut current_segment_start = 0;

    for token in tokens {
        let token_highlighting = highlighting_for_token(&token.kind);
        let error = error_span.contains(token.span);

        if token_highlighting == current_segment_highlighting && error == current_error {
            continue;
        }

        let token_start = token.span.start_byte;

        if current_error && error_span.end_byte < token_start {
            // ensure we do not extend the error over the whitespace between it and the next token
            segments.push(ScholleCodeSegment {
                segment: &code[current_segment_start..error_span.end_byte],
                highlighting: current_segment_highlighting,
                error: true,
            });

            segments.push(ScholleCodeSegment {
                segment: &code[error_span.end_byte..token_start],
                highlighting: Highlighting::Ordinary,
                error: false,
            })
        }
        else {
            segments.push(ScholleCodeSegment {
                segment: &code[current_segment_start..token_start],
                highlighting: current_segment_highlighting,
                error: current_error,
            });
        }

        current_segment_highlighting = token_highlighting;
        current_segment_start = token_start;
        current_error = error;
    }

    if current_segment_start < code.len() {
        segments.push(ScholleCodeSegment {
            segment: &code[current_segment_start..],
            highlighting: current_segment_highlighting,
            error: current_error,
        });
    }

    let unexpected_end_of_code = error_span.start_byte == code.len();

    if code.ends_with('\n') || unexpected_end_of_code {
        // ensures the last line has text so it is not omitted by <pre>
        segments.push(ScholleCodeSegment {
            segment: " ",
            highlighting: current_segment_highlighting,
            error: unexpected_end_of_code,
        });
    }

    segments
}

fn format_error(error: &ScholleError) -> String {
    // TODO do we want to implement this kind of formatting in Scholle directly?

    match error {
        ScholleError::Lexer(LexerError { kind, .. }) => match kind {
            LexerErrorKind::IntegerOverflow(value) => format!(
                "Integer literal exceeds maximum value: {}\nInteger literals must be at most {}",
                value,
                i64::MAX
            ),
            LexerErrorKind::InvalidCharacter(c) => format!("Invalid character: '{}'", c),
        },
        ScholleError::Parse(ParseError::UnexpectedToken(token)) => {
            format!("Unexpected token: '{}'", token.kind)
        },
        ScholleError::Context(context_error) => match context_error {
            ContextError::UnresolvedReference { identifier, .. } => {
                format!("Unresolved reference: '{}'", identifier)
            },
            ContextError::TypeError {
                actual_type,
                expected_types,
                ..
            } => {
                let expected_types_display = EnumerationDisplay::new(expected_types, "or");
                let expected_float = expected_types == &[ExpectedType::Type(Type::Float)];
                let expected_int = expected_types == &[ExpectedType::Type(Type::Integer)];
                let addition = if actual_type == &Type::Float && expected_int {
                    "\nUse `as_int(...)` to convert to integer"
                }
                else if actual_type == &Type::Integer && expected_float {
                    "\nUse `as_float(...)` to convert to float"
                }
                else {
                    ""
                };

                format!(
                    "Type error: expected {}, but found {}{}",
                    expected_types_display, actual_type, addition
                )
            },
            ContextError::CardinalityError {
                parameter_count,
                argument_count,
                ..
            } => format!(
                "Incorrect number of arguments: expected {}, but found {}",
                parameter_count, argument_count
            ),
        },
        ScholleError::Runtime(RuntimeError { kind, .. }) => match kind {
            RuntimeErrorKind::ArithmeticOverflow => "Arithmetic overflow\nOverflowing integer \
                operations are forbidden to avoid unexpected evaluations"
                .to_owned(),
            RuntimeErrorKind::DivideByZero => "Division by zero".to_owned(),
            &RuntimeErrorKind::InvalidResult(result) => {
                let result = if result == f64::INFINITY {
                    "Infinity".to_owned()
                }
                else if result == f64::NEG_INFINITY {
                    "-Infinity".to_owned()
                }
                else {
                    result.to_string()
                };

                format!(
                    "Invalid result: {}\nThis likely indicates an invalid arithmetic operation \
                    somewhere in your code, such as a division by zero.",
                    result
                )
            },
        },
    }
}

fn render_scholle_code_segment(segment: ScholleCodeSegment) -> Element {
    rsx! {
        span {
            class: segment.class(),

            { segment.segment }
        }
    }
}

fn sync_scroll() {
    // TODO reminder to check for ways to do this without JS

    document::eval(
        r#"
        const textarea = document.querySelector('.scholle-input-textarea textarea');
        const pre = document.querySelector('.scholle-input-textarea pre');

        pre.scrollTop = textarea.scrollTop;
        pre.scrollLeft = textarea.scrollLeft;
    "#,
    );
}

#[component]
pub fn ScholleInput(
    onnewevaluator: EventHandler<ScholleEvaluator>,
    error_signal: Signal<Option<ScholleError>>,
) -> Element {
    let mut code_signal = use_signal(|| DEFAULT_SCHOLLE_CODE.to_owned());

    rsx! {
        div {
            class: "scholle-input center-box",

            div {
                class: "scholle-input-textarea",

                // Trick for syntax highlighting:
                // Render a separate <pre><code>...</code></pre> under the textarea and hide the
                // text of the textarea.
                // Source: https://css-tricks.com/creating-an-editable-textarea-that-supports-syntax-highlighted-code/

                pre {
                    aria_hidden: true,

                    code {
                        for segment in to_segments(
                            &code_signal.read(),
                            error_signal.read().as_ref(),
                        ) {
                            { render_scholle_code_segment(segment) }
                        }
                    }
                }

                textarea {
                    spellcheck: false,

                    oninput: move |event| {
                        let code = event.value();
                        code_signal.set(code.clone());

                        match ScholleEvaluator::new(code) {
                            Ok(evaluator) => {
                                error_signal.set(None);
                                onnewevaluator.call(evaluator)
                            },
                            Err(e) => error_signal.set(Some(e.into()))
                        };

                        sync_scroll();
                    },

                    onscroll: move |_| sync_scroll(),

                    { format!("{}", code_signal.read()) }
                }
            }

            div {
                class: "help",

                a {
                    href: SCHOLLE_DOCUMENTATION_LINK,

                    "?"
                }
            }
        }

        if let Some(error) = error_signal.read().as_ref() {
            div {
                class: "error center-box",

                for (i, line) in format_error(error).lines().enumerate() {
                    if i > 0 {
                        br {}
                    }

                    { line }
                }
            }
        }
    }
}
