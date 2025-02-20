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
