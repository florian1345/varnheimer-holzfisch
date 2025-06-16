pub(crate) mod basic_fields;
pub(crate) mod flex_break;
pub(crate) mod skill_check_state;

#[macro_export]
macro_rules! clone_env {
    ($variable:ident -> $closure:expr) => {{
        let $variable = $variable.clone();
        $closure
    }};
}
