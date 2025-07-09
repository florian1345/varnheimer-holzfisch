mod focus;
mod form;
mod mouse;

use dioxus_core::prelude::EventHandler;
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
use futures::channel::mpsc;
use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};

pub use crate::event::focus::{FocusEventType, TestFocusData};
pub use crate::event::form::{FormEventType, TestFormData};
pub use crate::event::mouse::{MouseEventType, TestMouseData};
use crate::{NodeId, NodeRef, TestDom};

pub trait EventType: Sized {
    type Data: Default + 'static;

    fn name(self) -> &'static str;

    fn at(self, node: NodeRef) -> TestEvent<Self> {
        TestEvent::new(node, self)
    }
}

pub struct TestEvent<T: EventType> {
    pub(crate) node_id: NodeId,
    pub(crate) event_type: T,
    pub(crate) data: T::Data,
}

impl<T: EventType> TestEvent<T> {
    pub fn new(node: NodeRef, event_type: T) -> TestEvent<T> {
        TestEvent {
            node_id: node.id,
            event_type,
            data: T::Data::default(),
        }
    }

    pub fn with(self, data: T::Data) -> TestEvent<T> {
        TestEvent { data, ..self }
    }

    /// Same as [TestDom::raise], but with inverted syntax to allow chaining with other methods.
    ///
    /// # Arguments
    ///
    /// * `dom`: The [TestDom] in which to raise this event.
    pub fn raise(self, dom: &mut TestDom) {
        dom.raise(self)
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

#[derive(Clone)]
pub struct EventSender<T>(UnboundedSender<T>);

impl<T> PartialEq for EventSender<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.same_receiver(&other.0)
    }
}

impl<T: 'static> EventSender<T> {
    pub fn into_event_handler(self) -> EventHandler<T> {
        EventHandler::new(move |value| {
            self.0
                .unbounded_send(value)
                .unwrap_or_else(|err| panic!("failed to send event: {err}"));
        })
    }
}

pub type EventReceiver<T> = UnboundedReceiver<T>;

pub fn event_channel<T: 'static>() -> (EventSender<T>, EventReceiver<T>) {
    let (sender, receiver) = mpsc::unbounded();
    (EventSender(sender), receiver)
}
