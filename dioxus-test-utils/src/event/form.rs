use std::any::Any;
use std::collections::HashMap;
use std::sync::Arc;

use dioxus_html::{FileEngine, FormValue, HasFileData, HasFormData};

use crate::event::EventType;

#[derive(Clone)]
pub struct TestFormData {
    /// See [HasFormData::value].
    pub value: String,

    /// See [HasFormData::valid].
    pub valid: bool,

    /// See [HasFormData::values].
    pub values: HashMap<String, FormValue>,

    /// See [HasFileData::files].
    pub files: Option<Arc<dyn FileEngine>>,
}

impl Default for TestFormData {
    fn default() -> TestFormData {
        TestFormData {
            value: String::new(),
            valid: true,
            values: HashMap::new(),
            files: None,
        }
    }
}

impl HasFileData for TestFormData {
    fn files(&self) -> Option<Arc<dyn FileEngine>> {
        self.files.clone()
    }
}

impl HasFormData for TestFormData {
    fn value(&self) -> String {
        self.value.clone()
    }

    fn valid(&self) -> bool {
        self.valid
    }

    fn values(&self) -> HashMap<String, FormValue> {
        self.values.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub enum FormEventType {
    Change,
    Input,
    Invalid,
    Reset,
    Submit,
}

impl EventType for FormEventType {
    type Data = TestFormData;

    fn name(self) -> &'static str {
        match self {
            FormEventType::Change => "change",
            FormEventType::Input => "input",
            FormEventType::Invalid => "invalid",
            FormEventType::Reset => "reset",
            FormEventType::Submit => "submit",
        }
    }
}
