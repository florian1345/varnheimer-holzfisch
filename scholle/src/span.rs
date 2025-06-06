use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeSpan {
    pub start_byte: usize,
    pub end_byte: usize,
}

impl Display for CodeSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start_byte, self.end_byte)
    }
}
