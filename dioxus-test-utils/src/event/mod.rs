mod focus;
mod form;
mod mouse;

use std::any::Any;
use std::rc::Rc;

use dioxus_core::Event;
use dioxus_html::{
    AnimationData,
    ClipboardData,
    CompositionData,
    DragData,
    FocusData,
    FormData,
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
pub use crate::event::focus::{FocusEventType, TestFocusData};
pub use crate::event::form::{FormEventType, TestFormData};
pub use crate::event::mouse::{MouseEventType, TestMouseData};

pub trait EventType {
    type Data: 'static;

    fn name(self) -> &'static str;
}

impl<'dom> NodeRef<'dom> {
    pub fn trigger_with<T: EventType>(self, event_type: T, data: T::Data) {
        // TODO we should ensure that update() is called after each event
        //  potential idea: offer method NodeRef::event(...) -> Event and TestDom::trigger(Event)

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

    fn convert_focus_data(&self, event: &PlatformEventData) -> FocusData {
        convert::<TestFocusData, _>(event)
    }

    fn convert_form_data(&self, event: &PlatformEventData) -> FormData {
        convert::<TestFormData, _>(event)
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
