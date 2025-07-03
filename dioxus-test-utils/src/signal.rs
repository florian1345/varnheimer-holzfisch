use std::sync::{Arc, RwLock, RwLockReadGuard};

use dioxus_hooks::{use_effect, use_signal};
use dioxus_signals::{Signal, Writable};

use crate::TestDom;

struct TestSignalData<T: 'static> {
    value: Arc<RwLock<T>>,
    signal: Arc<RwLock<Option<Signal<T>>>>,
}

impl<T: 'static> Clone for TestSignalData<T> {
    fn clone(&self) -> TestSignalData<T> {
        TestSignalData {
            value: self.value.clone(),
            signal: self.signal.clone(),
        }
    }
}

impl<T: 'static> TestSignalData<T> {
    fn new(value: T) -> TestSignalData<T> {
        TestSignalData {
            value: Arc::new(RwLock::new(value)),
            signal: Arc::new(RwLock::new(None)),
        }
    }
}

pub struct TestSignalAccess<T: 'static>(TestSignalData<T>);

impl<T> TestSignalAccess<T> {
    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.value.read().unwrap()
    }

    /// Writes the given value in the signal linked to this access and updates the DOM.
    ///
    /// # Arguments
    ///
    /// * `value`: The value to assign to the signal.
    /// * `dom`: The [TestDom] in which an element uses the linked signal. This ensures updating the
    ///   DOM is not forgotten, which could lead to unexpected errors.
    ///
    /// # Panics
    ///
    /// If no signal has been linked to this access.
    pub fn set(&self, value: T, dom: &mut TestDom) {
        self.0
            .signal
            .write()
            .unwrap()
            .as_mut()
            .expect("test signal has not yet been linked")
            .set(value);
        dom.update();
    }
}

impl<T: Clone> TestSignalAccess<T> {
    pub fn get(&self) -> T {
        self.read().clone()
    }
}

/// Provides utility for filling [Signal] properties of tested components, validating any changes to
/// those signals made by tested components, and simulating changes to those signals from the
/// outside.
pub struct TestSignal<T: 'static>(TestSignalData<T>);

impl<T: 'static> TestSignal<T> {
    pub fn new(value: T) -> TestSignal<T> {
        TestSignal(TestSignalData::new(value))
    }

    pub fn with_access(value: T) -> (TestSignal<T>, TestSignalAccess<T>) {
        let test_signal = TestSignal::new(value);
        let access = TestSignalAccess(test_signal.0.clone());

        (test_signal, access)
    }
}

impl<T: 'static> Clone for TestSignal<T> {
    fn clone(&self) -> TestSignal<T> {
        TestSignal(self.0.clone())
    }
}

impl<T: Clone> TestSignal<T> {
    pub fn into_signal(self) -> Signal<T> {
        let signal = use_signal(|| self.0.value.read().unwrap().clone());
        use_effect(move || *self.0.value.write().unwrap() = signal());
        *self.0.signal.write().unwrap() = Some(signal);
        signal
    }
}

impl<T> PartialEq for TestSignal<T> {
    fn eq(&self, other: &TestSignal<T>) -> bool {
        Arc::as_ptr(&self.0.value) == Arc::as_ptr(&other.0.value)
    }
}
