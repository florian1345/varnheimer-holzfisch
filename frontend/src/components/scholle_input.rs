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
use crate::components::flex_break::FlexBreak;
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

            current_segment_start = error_span.end_byte;
            current_segment_highlighting = token_highlighting;
            current_error = false;
            continue;
        }

        segments.push(ScholleCodeSegment {
            segment: &code[current_segment_start..token_start],
            highlighting: current_segment_highlighting,
            error: current_error,
        });

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
    evaluator: Signal<Option<ScholleEvaluator>>,
    error: Signal<Option<ScholleError>>,
) -> Element {
    let mut code_signal = use_signal(|| DEFAULT_SCHOLLE_CODE.to_owned());

    rsx! {
        div {
            class: "scholle-input center-box",

            h3 {
                class: "center-box",
                "Outcome evaluation"
            }

            FlexBreak {}

            div {
                class: "scholle-input-textarea",

                // Trick for syntax highlighting:
                // Render a separate <pre><code>...</code></pre> under the textarea and hide the
                // text of the textarea.
                // Source: https://css-tricks.com/creating-an-editable-textarea-that-supports-syntax-highlighted-code/

                pre {
                    aria_hidden: true,

                    code {
                        for segment in to_segments(&code_signal(), error().as_ref()) {
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
                            Ok(new_evaluator) => {
                                error.set(None);
                                evaluator.set(Some(new_evaluator));
                            },
                            Err(e) => {
                                error.set(Some(e.into()));
                                evaluator.set(None);
                            },
                        }

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

        if let Some(error) = error().as_ref() {
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

#[cfg(test)]
mod tests {
    use dioxus_test_utils::event::{EventType, FormEventType, TestFormData};
    use dioxus_test_utils::signal::{TestSignal, TestSignalAccess};
    use dioxus_test_utils::{Find, NodeRef, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    fn mount() -> (
        TestDom,
        TestSignalAccess<Option<ScholleEvaluator>>,
        TestSignalAccess<Option<ScholleError>>,
    ) {
        let (evaluator, evaluator_validator) = TestSignal::with_access(None);
        let (error, error_validator) = TestSignal::with_access(None);

        #[component]
        fn Wrapper(
            evaluator: TestSignal<Option<ScholleEvaluator>>,
            error: TestSignal<Option<ScholleError>>,
        ) -> Element {
            rsx! {
                ScholleInput {
                    evaluator: evaluator.into_signal(),
                    error: error.into_signal(),
                }
            }
        }

        let dom = TestDom::new_with_props(Wrapper, WrapperProps { evaluator, error });

        (dom, evaluator_validator, error_validator)
    }

    fn mount_with_code(
        code: &str,
    ) -> (
        TestDom,
        TestSignalAccess<Option<ScholleEvaluator>>,
        TestSignalAccess<Option<ScholleError>>,
    ) {
        let (mut dom, evaluator, error) = mount();

        FormEventType::Input
            .at(dom.find(".scholle-input-textarea > textarea"))
            .with(TestFormData {
                value: code.to_owned(),
                ..Default::default()
            })
            .raise(&mut dom);

        assert_that!(dom.find(".scholle-input-textarea > textarea")).contains_only_text(code);

        (dom, evaluator, error)
    }

    struct ExpectedSegment {
        segment: &'static str,
        classes: Vec<&'static str>,
    }

    fn expected_segment(
        segment: &'static str,
        classes: impl IntoIterator<Item = &'static str>,
    ) -> ExpectedSegment {
        ExpectedSegment {
            segment,
            classes: classes.into_iter().collect(),
        }
    }

    fn assert_segments_match(nodes: Vec<NodeRef>, expected_segments: Vec<ExpectedSegment>) {
        assert_that!(&nodes).has_length(expected_segments.len());

        for (segment_node, expected_segment) in nodes.into_iter().zip(expected_segments) {
            assert_that!(segment_node)
                .contains_only_text(expected_segment.segment)
                .has_exactly_classes(expected_segment.classes);
        }
    }

    #[rstest]
    #[case::int_literal("1", [expected_segment("1", ["scholle-number"])])]
    #[case::float_literal("123.456", [expected_segment("123.456", ["scholle-number"])])]
    #[case::single_segment_operation(
        "quality_level + remaining_fate_points",
        [expected_segment("quality_level + remaining_fate_points", ["scholle-ordinary"])]
    )]
    #[case::multi_segment_operation(
        "if quality_level >= 2 then quality_level - 1 else quality_level",
        [
            expected_segment("if ", ["scholle-keyword"]),
            expected_segment("quality_level >= ", ["scholle-ordinary"]),
            expected_segment("2 ", ["scholle-number"]),
            expected_segment("then ", ["scholle-keyword"]),
            expected_segment("quality_level - ", ["scholle-ordinary"]),
            expected_segment("1 ", ["scholle-number"]),
            expected_segment("else ", ["scholle-keyword"]),
            expected_segment("quality_level", ["scholle-ordinary"]),
        ]
    )]
    #[case::trailing_whitespace(
        "1.0 * 2.0  ",
        [
            expected_segment("1.0 ", ["scholle-number"]),
            expected_segment("* ", ["scholle-ordinary"]),
            expected_segment("2.0  ", ["scholle-number"]),
        ]
    )]
    #[case::trailing_newline(
        "1.0\n",
        [
            expected_segment("1.0\n", ["scholle-number"]),
            expected_segment(" ", ["scholle-ordinary"]),
        ]
    )]
    #[case::multiple_keywords(
        "if true then 1 else 0",
        [
            expected_segment("if true then ", ["scholle-keyword"]),
            expected_segment("1 ", ["scholle-number"]),
            expected_segment("else ", ["scholle-keyword"]),
            expected_segment("0", ["scholle-number"]),
        ]
    )]
    #[case::type_keywords(
        "let \
            f = (x: int, y: float, z: bool) -> if z then as_float(x) / y else 0.0 \
        in \
            f(quality_level, 2.0, is_success)",
        [
            expected_segment("let ", ["scholle-keyword"]),
            expected_segment("f = (x: ", ["scholle-ordinary"]),
            expected_segment("int", ["scholle-keyword"]),
            expected_segment(", y: ", ["scholle-ordinary"]),
            expected_segment("float", ["scholle-keyword"]),
            expected_segment(", z: ", ["scholle-ordinary"]),
            expected_segment("bool", ["scholle-keyword"]),
            expected_segment(") -> ", ["scholle-ordinary"]),
            expected_segment("if ", ["scholle-keyword"]),
            expected_segment("z ", ["scholle-ordinary"]),
            expected_segment("then ", ["scholle-keyword"]),
            expected_segment("as_float(x) / y ", ["scholle-ordinary"]),
            expected_segment("else ", ["scholle-keyword"]),
            expected_segment("0.0 ", ["scholle-number"]),
            expected_segment("in ", ["scholle-keyword"]),
            expected_segment("f(quality_level, ", ["scholle-ordinary"]),
            expected_segment("2.0", ["scholle-number"]),
            expected_segment(", is_success)", ["scholle-ordinary"]),
        ]
    )]
    fn scholle_input_ok(
        #[case] code: &str,
        #[case] expected_segments: impl IntoIterator<Item = ExpectedSegment>,
    ) {
        let (dom, evaluator, error) = mount_with_code(code);

        assert_that!(evaluator.get()).is_some();
        assert_that!(error.get()).is_none();

        let segment_nodes = dom.find_all(".scholle-input-textarea > pre > code > span");
        let expected_segments = expected_segments.into_iter().collect::<Vec<_>>();

        assert_segments_match(segment_nodes, expected_segments);
        assert_that!(dom.find_all(".error")).is_empty();
    }

    fn expect_error(
        dom: &TestDom,
        expected_error_message_lines: impl IntoIterator<Item = &'static str>,
    ) {
        let error_node = dom.find(".error");
        let error_message_nodes = error_node.non_placeholder_children();
        let expected_error_message_lines =
            expected_error_message_lines.into_iter().collect::<Vec<_>>();

        assert_that!(&error_message_nodes).has_length(expected_error_message_lines.len() * 2 - 1);
        assert_that!(error_node.find_all("br")).has_length(expected_error_message_lines.len() - 1);

        for i in 0..expected_error_message_lines.len() {
            assert_that!(error_message_nodes[i * 2]).is_text(expected_error_message_lines[i]);
        }
    }

    #[rstest]
    #[case::emtpy(
        "",
        [expected_segment(" ", ["scholle-ordinary", "scholle-error"])],
        ["Unexpected token: '<end of code>'"]
    )]
    #[case::invalid_character_start(
        "# abc",
        [
            expected_segment("#", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" abc", ["scholle-ordinary"]),
        ],
        ["Invalid character: '#'"]
    )]
    #[case::invalid_character_middle(
        "xyz $ abc",
        [
            expected_segment("xyz ", ["scholle-ordinary"]),
            expected_segment("$", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" abc", ["scholle-ordinary"]),
        ],
        ["Invalid character: '$'"]
    )]
    #[case::invalid_character_end(
        "xyz @",
        [
            expected_segment("xyz ", ["scholle-ordinary"]),
            expected_segment("@", ["scholle-ordinary", "scholle-error"]),
        ],
        ["Invalid character: '@'"]
    )]
    #[case::unexpected_end_of_code(
        "1 + ",
        [
            expected_segment("1 ", ["scholle-number"]),
            expected_segment("+ ", ["scholle-ordinary"]),
            expected_segment(" ", ["scholle-ordinary", "scholle-error"]),
        ],
        ["Unexpected token: '<end of code>'"]
    )]
    #[case::unexpected_token(
        "1,2",
        [
            expected_segment("1", ["scholle-number"]),
            expected_segment(",", ["scholle-ordinary", "scholle-error"]),
            expected_segment("2", ["scholle-number"]),
        ],
        ["Unexpected token: ','"]
    )]
    #[case::unexpected_token_interrupting_sequence_of_equal_highlighting(
        "quality_level + * + quality_level",
        [
            expected_segment("quality_level + ", ["scholle-ordinary"]),
            expected_segment("*", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" + quality_level", ["scholle-ordinary"]),
        ],
        ["Unexpected token: '*'"]
    )]
    #[case::type_error_single_segment(
        "if quality_level then 1 else 0",
        [
            expected_segment("if ", ["scholle-keyword"]),
            expected_segment("quality_level", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" then ", ["scholle-keyword"]),
            expected_segment("1 ", ["scholle-number"]),
            expected_segment("else ", ["scholle-keyword"]),
            expected_segment("0", ["scholle-number"]),
        ],
        ["Type error: expected bool, but found int"],
    )]
    #[case::type_eror_multiple_segments(
        "(true | false) + quality_level",
        [
            expected_segment("(", ["scholle-ordinary", "scholle-error"]),
            expected_segment("true ", ["scholle-keyword", "scholle-error"]),
            expected_segment("| ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("false", ["scholle-keyword", "scholle-error"]),
            expected_segment(")", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" + quality_level", ["scholle-ordinary"]),
        ],
        ["Type error: expected int or float, but found bool"],
    )]
    #[case::type_error_three_legal_types(
        "pow == 1",
        [
            expected_segment("pow", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" == ", ["scholle-ordinary"]),
            expected_segment("1", ["scholle-number"]),
        ],
        ["Type error: expected bool, int, or float, but found (float, float) -> float"]
    )]
    #[case::cast_to_int_hint(
        "1 + 1.0",
        [
            expected_segment("1 ", ["scholle-number"]),
            expected_segment("+ ", ["scholle-ordinary"]),
            expected_segment("1.0", ["scholle-number", "scholle-error"]),
        ],
        [
            "Type error: expected int, but found float",
            "Use `as_int(...)` to convert to integer",
        ]
    )]
    #[case::cast_to_float_hint(
        "1.0 + 1",
        [
            expected_segment("1.0 ", ["scholle-number"]),
            expected_segment("+ ", ["scholle-ordinary"]),
            expected_segment("1", ["scholle-number", "scholle-error"]),
        ],
        [
            "Type error: expected float, but found int",
            "Use `as_float(...)` to convert to float",
        ]
    )]
    #[case::unresolved_reference(
        "x",
        [expected_segment("x", ["scholle-ordinary", "scholle-error"])],
        ["Unresolved reference: 'x'"]
    )]
    #[case::cardinality(
        "pow(1.0)",
        [
            expected_segment("pow(", ["scholle-ordinary", "scholle-error"]),
            expected_segment("1.0", ["scholle-number", "scholle-error"]),
            expected_segment(")", ["scholle-ordinary", "scholle-error"]),
        ],
        ["Incorrect number of arguments: expected 2, but found 1"]
    )]
    fn scholle_input_error_on_input(
        #[case] code: &str,
        #[case] expected_segments: impl IntoIterator<Item = ExpectedSegment>,
        #[case] expected_error_message_lines: impl IntoIterator<Item = &'static str>,
    ) {
        let (dom, evaluator, error) = mount_with_code(code);

        assert_that!(evaluator.get()).is_none();
        assert_that!(error.get()).is_some();

        let segment_nodes = dom.find_all(".scholle-input-textarea > pre > code > span");
        let expected_segments = expected_segments.into_iter().collect::<Vec<_>>();

        assert_segments_match(segment_nodes, expected_segments);
        expect_error(&dom, expected_error_message_lines);
    }

    #[rstest]
    #[case::division_by_zero(
        "100 - 100 / quality_level + 1",
        RuntimeError {
            kind: RuntimeErrorKind::DivideByZero,
            span: (6..25).into(),
        },
        [
            expected_segment("100 ", ["scholle-number"]),
            expected_segment("- ", ["scholle-ordinary"]),
            expected_segment("100 ", ["scholle-number", "scholle-error"]),
            expected_segment("/ quality_level", ["scholle-ordinary", "scholle-error"]),
            expected_segment(" + ", ["scholle-ordinary"]),
            expected_segment("1", ["scholle-number"]),
        ],
        ["Division by zero"]
    )]
    #[case::arithmetic_overflow(
        "as_int(pow(10.0, 100.0)) + 1",
        RuntimeError {
            kind: RuntimeErrorKind::ArithmeticOverflow,
            span: (0..28).into(),
        },
        [
            expected_segment("as_int(pow(", ["scholle-ordinary", "scholle-error"]),
            expected_segment("10.0", ["scholle-number", "scholle-error"]),
            expected_segment(", ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("100.0", ["scholle-number", "scholle-error"]),
            expected_segment(")) + ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("1", ["scholle-number", "scholle-error"]),
        ],
        [
            "Arithmetic overflow",
            "Overflowing integer operations are forbidden to avoid unexpected evaluations",
        ]
    )]
    #[case::invalid_result_infinity(
        "1.0 / 0.0",
        RuntimeError {
            kind: RuntimeErrorKind::InvalidResult(f64::INFINITY),
            span: (0..9).into(),
        },
        [
            expected_segment("1.0 ", ["scholle-number", "scholle-error"]),
            expected_segment("/ ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("0.0", ["scholle-number", "scholle-error"]),
        ],
        [
            "Invalid result: Infinity",
            "This likely indicates an invalid arithmetic operation somewhere in your code, such as \
                a division by zero.",
        ]
    )]
    #[case::invalid_result_neg_infinity(
        "-1.0 / 0.0",
        RuntimeError {
            kind: RuntimeErrorKind::InvalidResult(f64::NEG_INFINITY),
            span: (0..10).into(),
        },
        [
            expected_segment("-", ["scholle-ordinary", "scholle-error"]),
            expected_segment("1.0 ", ["scholle-number", "scholle-error"]),
            expected_segment("/ ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("0.0", ["scholle-number", "scholle-error"]),
        ],
        [
            "Invalid result: -Infinity",
            "This likely indicates an invalid arithmetic operation somewhere in your code, such as \
                a division by zero.",
        ]
    )]
    #[case::invalid_result_nan(
        "pow(-1.0, 0.5)",
        RuntimeError {
            kind: RuntimeErrorKind::InvalidResult(f64::NAN),
            span: (0..14).into(),
        },
        [
            expected_segment("pow(-", ["scholle-ordinary", "scholle-error"]),
            expected_segment("1.0", ["scholle-number", "scholle-error"]),
            expected_segment(", ", ["scholle-ordinary", "scholle-error"]),
            expected_segment("0.5", ["scholle-number", "scholle-error"]),
            expected_segment(")", ["scholle-ordinary", "scholle-error"]),
        ],
        [
            "Invalid result: NaN",
            "This likely indicates an invalid arithmetic operation somewhere in your code, such as \
                a division by zero.",
        ]
    )]
    fn scholle_input_runtime_error(
        #[case] code: &str,
        #[case] error: RuntimeError,
        #[case] expected_segments: impl IntoIterator<Item = ExpectedSegment>,
        #[case] expected_error_message_lines: impl IntoIterator<Item = &'static str>,
    ) {
        let (mut dom, _, error_access) = mount_with_code(code);
        error_access.set(Some(error.into()), &mut dom);

        let segment_nodes = dom.find_all(".scholle-input-textarea > pre > code > span");
        let expected_segments = expected_segments.into_iter().collect::<Vec<_>>();

        assert_segments_match(segment_nodes, expected_segments);
        expect_error(&dom, expected_error_message_lines);
    }

    #[test]
    fn scholle_input_renders_help_button() {
        let (dom, _, _) = mount();

        assert_that!(dom.find("div.help > a")).has_attribute("href", SCHOLLE_DOCUMENTATION_LINK);
    }
}
