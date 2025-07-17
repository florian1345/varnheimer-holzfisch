use std::any::Any;

use dioxus_html::geometry::{ClientPoint, ElementPoint, PagePoint, ScreenPoint};
use dioxus_html::input_data::{MouseButton, MouseButtonSet};
use dioxus_html::point_interaction::{
    InteractionElementOffset,
    InteractionLocation,
    ModifiersInteraction,
    PointerInteraction,
};
use dioxus_html::{HasMouseData, Modifiers};

use crate::event::EventType;

#[derive(Clone)]
pub struct TestMouseData {
    pub trigger_button: Option<MouseButton>,
    pub held_buttons: MouseButtonSet,
    pub element_coordinates: ElementPoint,
    pub client_coordinates: ClientPoint,
    pub screen_coordinates: ScreenPoint,
    pub page_coordinates: PagePoint,
    pub modifiers: Modifiers,
}

impl Default for TestMouseData {
    fn default() -> Self {
        TestMouseData {
            trigger_button: Some(MouseButton::Primary),
            held_buttons: MouseButtonSet::default(),
            element_coordinates: ElementPoint::default(),
            client_coordinates: ClientPoint::default(),
            screen_coordinates: ScreenPoint::default(),
            page_coordinates: PagePoint::default(),
            modifiers: Modifiers::default(),
        }
    }
}

impl PointerInteraction for TestMouseData {
    fn trigger_button(&self) -> Option<MouseButton> {
        self.trigger_button
    }

    fn held_buttons(&self) -> MouseButtonSet {
        self.held_buttons
    }
}

impl InteractionElementOffset for TestMouseData {
    fn element_coordinates(&self) -> ElementPoint {
        self.element_coordinates
    }
}

impl InteractionLocation for TestMouseData {
    fn client_coordinates(&self) -> ClientPoint {
        self.client_coordinates
    }

    fn screen_coordinates(&self) -> ScreenPoint {
        self.screen_coordinates
    }

    fn page_coordinates(&self) -> PagePoint {
        self.page_coordinates
    }
}

impl ModifiersInteraction for TestMouseData {
    fn modifiers(&self) -> Modifiers {
        self.modifiers
    }
}

impl HasMouseData for TestMouseData {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MouseEventType {
    Click,
    ContextMenu,
    DoubleClick,
    MouseDown,
    MouseEnter,
    MouseLeave,
    MouseMove,
    MouseOut,
    MouseOver,
    MouseUp,
}

impl EventType for MouseEventType {
    type Data = TestMouseData;

    fn name(self) -> &'static str {
        match self {
            MouseEventType::Click => "click",
            MouseEventType::ContextMenu => "contextmenu",
            MouseEventType::DoubleClick => "doubleclick",
            MouseEventType::MouseDown => "mousedown",
            MouseEventType::MouseEnter => "mouseenter",
            MouseEventType::MouseLeave => "mouseleave",
            MouseEventType::MouseMove => "mousemove",
            MouseEventType::MouseOut => "mouseout",
            MouseEventType::MouseOver => "mouseover",
            MouseEventType::MouseUp => "mouseup",
        }
    }
}
