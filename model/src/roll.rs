use std::num::NonZeroU8;

pub const DICE_SIDES: usize = 20;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Roll(NonZeroU8);

impl Roll {

    pub const MIN: Roll = Roll(NonZeroU8::new(1).unwrap());
    pub const MAX: Roll = Roll(NonZeroU8::new(20).unwrap());
    pub const ALL: [Roll; DICE_SIDES] = {
        let mut result = [Roll::MIN; DICE_SIDES];
        let mut i = 0;

        while i < DICE_SIDES {
            result[i] = Roll(NonZeroU8::new((i + 1) as u8).unwrap());
            i += 1;
        }

        result
    };

    pub fn new(value: u8) -> Option<Roll> {
        if value as usize > DICE_SIDES {
            return None;
        }

        NonZeroU8::new(value).map(Roll)
    }

    pub fn as_u8(self) -> u8 {
        self.0.get()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    use kernal::prelude::*;

    use rstest::rstest;

    #[rstest]
    #[case::zero(0)]
    #[case::too_large(21)]
    fn new_roll_with_invalid_value_is_none(#[case] value: u8) {
        assert_that!(Roll::new(value)).is_none();
    }

    #[rstest]
    #[case::min(1)]
    #[case::middle(10)]
    #[case::max(20)]
    fn new_roll_with_valid_value_is_some(#[case] value: u8) {
        let roll = Roll::new(value);

        assert_that!(roll).is_some();
        assert_that!(roll.unwrap().as_u8()).is_equal_to(value);
    }

    #[test]
    fn all_rolls_correctly_initialized() {
        let all_rolls_as_u8 = Roll::ALL.iter().copied().map(Roll::as_u8).collect::<Vec<_>>();

        assert_that!(all_rolls_as_u8).contains_exactly_in_given_order(1..=20);
    }
}
