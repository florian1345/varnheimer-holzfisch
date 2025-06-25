use std::any::Any;

use dioxus_html::HasFocusData;

use crate::event::EventType;

#[derive(Clone, Default)]
pub struct TestFocusData;

impl HasFocusData for TestFocusData {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub enum FocusEventType {
    Focus,
    FocusOut,
    FocusIn,
    Blur,
}

impl EventType for FocusEventType {
    type Data = TestFocusData;

    fn name(self) -> &'static str {
        match self {
            FocusEventType::Focus => "focus",
            FocusEventType::FocusOut => "focusout",
            FocusEventType::FocusIn => "focusin",
            FocusEventType::Blur => "blur",
        }
    }
}
