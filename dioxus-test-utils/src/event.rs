use std::any::Any;
use std::rc::Rc;

use dioxus_core::Event;
use dioxus_html::geometry::{ClientPoint, ElementPoint, PagePoint, ScreenPoint};
use dioxus_html::input_data::{MouseButton, MouseButtonSet};
use dioxus_html::point_interaction::{
    InteractionElementOffset,
    InteractionLocation,
    ModifiersInteraction,
    PointerInteraction,
};
use dioxus_html::prelude::Modifiers;
use dioxus_html::{
    AnimationData,
    ClipboardData,
    CompositionData,
    DragData,
    FocusData,
    FormData,
    HasMouseData,
    HtmlEventConverter,
    ImageData,
    KeyboardData,
    MediaData,
    MountedData,
    MouseData,
    PlatformEventData,
    PointerData,
    ResizeData,
    ScrollData,
    SelectionData,
    ToggleData,
    TouchData,
    TransitionData,
    VisibleData,
    WheelData,
};

use crate::NodeRef;

#[derive(Clone, Copy, Debug, PartialEq)]
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

pub trait EventType {
    type Data: 'static;

    fn name(self) -> &'static str;
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

impl<'dom> NodeRef<'dom> {
    pub fn trigger_with<T: EventType>(self, event_type: T, data: T::Data) {
        let platform_event_data = PlatformEventData::new(Box::new(data));
        let event = Event::new(Rc::new(platform_event_data) as Rc<dyn Any>, true);
        let element_id = self
            .nodes
            .get_element_id(self.id)
            .expect("cannot handle event for node without element ID");
        self.virtual_dom
            .runtime()
            .handle_event(event_type.name(), event, element_id);
    }

    pub fn trigger<T: EventType<Data: Default>>(self, event_type: T) {
        self.trigger_with(event_type, T::Data::default())
    }
}

fn convert<TestDataT, ConvertedT>(event: &PlatformEventData) -> ConvertedT
where
    TestDataT: Clone + 'static,
    ConvertedT: From<TestDataT>,
{
    event.downcast::<TestDataT>().cloned().unwrap().into()
}

pub(crate) struct TestHtmlEventConverter;

impl HtmlEventConverter for TestHtmlEventConverter {
    fn convert_animation_data(&self, _: &PlatformEventData) -> AnimationData {
        todo!()
    }

    fn convert_clipboard_data(&self, _: &PlatformEventData) -> ClipboardData {
        todo!()
    }

    fn convert_composition_data(&self, _: &PlatformEventData) -> CompositionData {
        todo!()
    }

    fn convert_drag_data(&self, _: &PlatformEventData) -> DragData {
        todo!()
    }

    fn convert_focus_data(&self, _: &PlatformEventData) -> FocusData {
        todo!()
    }

    fn convert_form_data(&self, _: &PlatformEventData) -> FormData {
        todo!()
    }

    fn convert_image_data(&self, _: &PlatformEventData) -> ImageData {
        todo!()
    }

    fn convert_keyboard_data(&self, _: &PlatformEventData) -> KeyboardData {
        todo!()
    }

    fn convert_media_data(&self, _: &PlatformEventData) -> MediaData {
        todo!()
    }

    fn convert_mounted_data(&self, _: &PlatformEventData) -> MountedData {
        todo!()
    }

    fn convert_mouse_data(&self, event: &PlatformEventData) -> MouseData {
        convert::<TestMouseData, _>(event)
    }

    fn convert_pointer_data(&self, _: &PlatformEventData) -> PointerData {
        todo!()
    }

    fn convert_resize_data(&self, _: &PlatformEventData) -> ResizeData {
        todo!()
    }

    fn convert_scroll_data(&self, _: &PlatformEventData) -> ScrollData {
        todo!()
    }

    fn convert_selection_data(&self, _: &PlatformEventData) -> SelectionData {
        todo!()
    }

    fn convert_toggle_data(&self, _: &PlatformEventData) -> ToggleData {
        todo!()
    }

    fn convert_touch_data(&self, _: &PlatformEventData) -> TouchData {
        todo!()
    }

    fn convert_transition_data(&self, _: &PlatformEventData) -> TransitionData {
        todo!()
    }

    fn convert_visible_data(&self, _: &PlatformEventData) -> VisibleData {
        todo!()
    }

    fn convert_wheel_data(&self, _: &PlatformEventData) -> WheelData {
        todo!()
    }
}
