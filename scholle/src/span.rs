use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Range;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CodeSpan {
    pub start_byte: usize,
    pub end_byte: usize,
}

impl CodeSpan {
    pub fn union(self, other: CodeSpan) -> CodeSpan {
        CodeSpan {
            start_byte: self.start_byte.min(other.start_byte),
            end_byte: self.end_byte.max(other.end_byte),
        }
    }
}

impl Display for CodeSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start_byte, self.end_byte)
    }
}

impl From<Range<usize>> for CodeSpan {
    fn from(value: Range<usize>) -> CodeSpan {
        CodeSpan {
            start_byte: value.start,
            end_byte: value.end,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use kernal::prelude::*;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::disjoint(2..5, 7..9, 2..9)]
    #[case::overlap(3..8, 5..10, 3..10)]
    #[case::sub_span(7..15, 9..12, 7..15)]
    fn code_span_union(
        #[case] span_1: Range<usize>,
        #[case] span_2: Range<usize>,
        #[case] expected: Range<usize>,
    ) {
        let code_span_1 = CodeSpan::from(span_1);
        let code_span_2 = CodeSpan::from(span_2);
        let expected = CodeSpan::from(expected);

        assert_that!(code_span_1.union(code_span_2)).is_equal_to(expected);
        assert_that!(code_span_2.union(code_span_1)).is_equal_to(expected);
    }
}
