use dioxus::prelude::*;
use scholle::lexer::TokenKind;
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

fn to_segments(code: &str) -> Vec<ScholleCodeSegment> {
    let Ok(tokens) = lexer::lex(code)
    else {
        return vec![ScholleCodeSegment {
            segment: code,
            highlighting: Highlighting::Ordinary,
        }];
    };

    let mut segments = vec![];
    let mut current_segment_highlighting = highlighting_for_token(&tokens[0].kind);
    let mut current_segment_start = 0;

    for token in tokens {
        let token_highlighting = highlighting_for_token(&token.kind);

        if token_highlighting == current_segment_highlighting {
            continue;
        }

        let token_start = token.span.start_byte;
        segments.push(ScholleCodeSegment {
            segment: &code[current_segment_start..token_start],
            highlighting: current_segment_highlighting,
        });

        current_segment_highlighting = token_highlighting;
        current_segment_start = token_start;
    }

    if current_segment_start < code.len() {
        segments.push(ScholleCodeSegment {
            segment: &code[current_segment_start..],
            highlighting: current_segment_highlighting,
        });
    }

    if code.ends_with('\n') {
        // ensures the last line has text so it is not omitted by <pre>
        segments.push(ScholleCodeSegment {
            segment: " ",
            highlighting: current_segment_highlighting,
        });
    }

    segments
}

fn render_scholle_code_segment(segment: ScholleCodeSegment) -> Element {
    rsx! {
        span {
            class: match segment.highlighting {
                Highlighting::Ordinary => "scholle-ordinary",
                Highlighting::Keyword => "scholle-keyword",
                Highlighting::Number => "scholle-number",
            },

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
pub fn ScholleInput(onnewevaluator: EventHandler<ScholleEvaluator>) -> Element {
    let mut code_signal = use_signal(|| DEFAULT_SCHOLLE_CODE.to_owned());
    let mut error_signal: Signal<Option<String>> = use_signal(|| None);

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
                        for segment in to_segments(&code_signal.read()) {
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
                            Err(e) => error_signal.set(Some(e.to_string()))
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

                { error }
            }
        }
    }
}
