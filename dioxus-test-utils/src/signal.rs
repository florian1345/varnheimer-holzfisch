use std::sync::{Arc, RwLock, RwLockReadGuard};

use dioxus_hooks::{use_effect, use_signal};
use dioxus_signals::Signal;

pub struct TestSignalValidator<T>(Arc<RwLock<T>>);

impl<T> TestSignalValidator<T> {
    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }
}

impl<T: Clone> TestSignalValidator<T> {
    pub fn get(&self) -> T {
        self.read().clone()
    }
}

#[derive(Clone)]
pub struct TestSignal<T>(Arc<RwLock<T>>);

impl<T> TestSignal<T> {
    pub fn new(value: T) -> TestSignal<T> {
        TestSignal(Arc::new(RwLock::new(value)))
    }

    pub fn with_validator(value: T) -> (TestSignal<T>, TestSignalValidator<T>) {
        let test_signal = TestSignal::new(value);
        let validator = TestSignalValidator(test_signal.0.clone());

        (test_signal, validator)
    }
}

impl<T: Clone> TestSignal<T> {
    pub fn into_signal(self) -> Signal<T> {
        let signal = use_signal(|| self.0.read().unwrap().clone());
        use_effect(move || *self.0.write().unwrap() = signal());
        signal
    }
}

impl<T> PartialEq for TestSignal<T> {
    fn eq(&self, other: &TestSignal<T>) -> bool {
        Arc::as_ptr(&self.0) == Arc::as_ptr(&other.0)
    }
}
