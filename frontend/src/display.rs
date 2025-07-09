use std::fmt;
use std::fmt::{Display, Formatter};

pub struct EnumerationDisplay<'enumeration, T> {
    enumeration: &'enumeration [T],
    conjunction: String,
}

impl<'enumeration, T> EnumerationDisplay<'enumeration, T> {
    pub fn new(
        enumeration: &'enumeration [T],
        conjunction: impl Into<String>,
    ) -> EnumerationDisplay<'enumeration, T> {
        EnumerationDisplay {
            enumeration,
            conjunction: conjunction.into(),
        }
    }
}

impl<'enumeration, T: Display> Display for EnumerationDisplay<'enumeration, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let len = self.enumeration.len();

        if len == 0 {
            Ok(())
        }
        else if len == 1 {
            write!(f, "{}", self.enumeration[0])
        }
        else if len == 2 {
            write!(
                f,
                "{} {} {}",
                self.enumeration[0], self.conjunction, self.enumeration[1]
            )
        }
        else {
            write!(
                f,
                "{}, {} {}",
                self.enumeration[0..(len - 1)]
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
                self.conjunction,
                self.enumeration[len - 1]
            )
        }
    }
}
