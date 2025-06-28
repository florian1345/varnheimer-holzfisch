use dioxus::prelude::*;
use scholle::error::ScholleError;
use scholle::lexer::TokenKind;
use scholle::span::CodeSpan;
use scholle::{ScholleEvaluator, lexer};

use crate::DEFAULT_SCHOLLE_CODE;

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

        if let Some(error) = error_signal.read().as_ref().cloned() {
            div {
                class: "error center-box",

                { format!("{}", error) }
            }
        }
    }
}
