use std::sync::LazyLock;

use model::check::modifier::{Aptitude, Modifier};
use model::check::outcome::{SkillCheckOutcome, SkillCheckOutcomeKind};
use model::skill::{QualityLevel, SkillPoints};

use crate::context::{DeclarationId, Type};
use crate::runtime::{BuiltinFunction, Value};

struct RawBuiltin {
    identifier: &'static str,
    typ: Type,
    constructor: Box<dyn Fn(&SkillCheckOutcome) -> Value + Send + Sync>,
}

impl RawBuiltin {
    fn to_context_builtin(&self, declaration_id: DeclarationId) -> ContextBuiltin {
        ContextBuiltin {
            identifier: self.identifier,
            typ: self.typ.clone(),
            declaration_id,
        }
    }

    fn to_runtime_builtin(
        &self,
        declaration_id: DeclarationId,
        outcome: &SkillCheckOutcome,
    ) -> RuntimeBuiltin {
        RuntimeBuiltin {
            declaration_id,
            value: (self.constructor)(outcome),
        }
    }

    fn new(
        identifier: &'static str,
        typ: Type,
        constructor: impl Fn(&SkillCheckOutcome) -> Value + Send + Sync + 'static,
    ) -> RawBuiltin {
        RawBuiltin {
            identifier,
            typ,
            constructor: Box::new(constructor),
        }
    }

    fn new_function<ImplT>(
        identifier: &'static str,
        parameter_types: impl IntoIterator<Item = Type>,
        return_type: Type,
        implementation: ImplT,
    ) -> RawBuiltin
    where
        ImplT: Fn(Arguments, &SkillCheckOutcome) -> Value + Clone + Send + Sync + 'static,
    {
        RawBuiltin {
            identifier,
            typ: Type::Function {
                parameter_types: parameter_types.into_iter().collect(),
                return_type: Box::new(return_type),
            },
            constructor: Box::new(move |outcome: &SkillCheckOutcome| {
                let outcome = outcome.clone();
                let implementation = implementation.clone();

                Value::BuiltinFunction(BuiltinFunction::new(move |arguments: Vec<Value>| {
                    implementation(Arguments(arguments), &outcome)
                }))
            }),
        }
    }
}

pub struct ContextBuiltin {
    pub identifier: &'static str,
    pub typ: Type,
    pub declaration_id: DeclarationId,
}

pub struct RuntimeBuiltin {
    pub declaration_id: DeclarationId,
    pub value: Value,
}

struct Arguments(Vec<Value>);

impl Arguments {
    fn get_int(&self, index: usize) -> i64 {
        let value = &self.0[index];

        if let &Value::Integer(i) = value {
            i
        }
        else {
            panic!("builtin-argument {:?} is not an integer", value);
        }
    }

    fn get_float(&self, index: usize) -> f64 {
        let value = &self.0[index];

        if let &Value::Float(f) = value {
            f
        }
        else {
            panic!("builtin-argument {:?} is not a float", value);
        }
    }
}

fn parameterized_modifier_count_accessor(
    identifier: &'static str,
    modifier_from_argument: impl Fn(i64) -> Option<Modifier> + Clone + Send + Sync + 'static,
) -> RawBuiltin {
    RawBuiltin::new_function(
        identifier,
        [Type::Integer],
        Type::Integer,
        move |arguments, outcome| {
            let argument = arguments.get_int(0);
            let remaining_aptitudes = modifier_from_argument(argument)
                .map(|modifier| outcome.remaining_modifiers.count_of(modifier))
                .unwrap_or(0);

            Value::Integer(remaining_aptitudes as i64)
        },
    )
}

pub const BUILTIN_COUNT: usize = 15;

static RAW_BUILTINS: LazyLock<[RawBuiltin; BUILTIN_COUNT]> = LazyLock::new(|| {
    [
        RawBuiltin::new_function("as_float", [Type::Integer], Type::Float, |arguments, _| {
            Value::Float(arguments.get_int(0) as f64)
        }),
        RawBuiltin::new_function("as_int", [Type::Float], Type::Integer, |arguments, _| {
            Value::Integer(arguments.get_float(0) as i64)
        }),
        RawBuiltin::new_function(
            "pow",
            [Type::Float, Type::Float],
            Type::Float,
            |arguments, _| Value::Float(arguments.get_float(0).powf(arguments.get_float(1))),
        ),
        RawBuiltin::new("quality_level", Type::Integer, |outcome| {
            let quality_level_u8 = outcome
                .quality_level()
                .map(QualityLevel::as_u8)
                .unwrap_or(0);

            Value::Integer(quality_level_u8 as i64)
        }),
        RawBuiltin::new("is_success", Type::Bool, |outcome| {
            Value::Bool(outcome.quality_level().is_some())
        }),
        RawBuiltin::new("is_critical_success", Type::Bool, |outcome| {
            let is_critical_success = matches!(
                outcome.kind,
                SkillCheckOutcomeKind::CriticalSuccess(_)
                    | SkillCheckOutcomeKind::SpectacularSuccess(_)
            );

            Value::Bool(is_critical_success)
        }),
        RawBuiltin::new("is_spectacular_success", Type::Bool, |outcome| {
            let is_spectacular_success =
                matches!(outcome.kind, SkillCheckOutcomeKind::SpectacularSuccess(_));

            Value::Bool(is_spectacular_success)
        }),
        RawBuiltin::new("is_failure", Type::Bool, |outcome| {
            Value::Bool(outcome.quality_level().is_none())
        }),
        RawBuiltin::new("is_critical_failure", Type::Bool, |outcome| {
            let is_critical_failure = outcome.kind == SkillCheckOutcomeKind::CriticalFailure
                || outcome.kind == SkillCheckOutcomeKind::SpectacularFailure;

            Value::Bool(is_critical_failure)
        }),
        RawBuiltin::new("is_spectacular_failure", Type::Bool, |outcome| {
            let is_spectacular_failure = outcome.kind == SkillCheckOutcomeKind::SpectacularFailure;

            Value::Bool(is_spectacular_failure)
        }),
        RawBuiltin::new("remaining_fate_points", Type::Integer, |outcome| {
            let remaining_fate_points = outcome.remaining_modifiers.count_of(Modifier::FatePoint);

            Value::Integer(remaining_fate_points as i64)
        }),
        parameterized_modifier_count_accessor("remaining_aptitudes", |aptitude_dice| {
            usize::try_from(aptitude_dice)
                .ok()
                .and_then(Aptitude::new)
                .map(Modifier::Aptitude)
        }),
        parameterized_modifier_count_accessor("remaining_extra_skill_points", |skill_points| {
            i32::try_from(skill_points)
                .ok()
                .map(SkillPoints::new)
                .map(Modifier::ExtraSkillPoints)
        }),
        parameterized_modifier_count_accessor(
            "remaining_extra_skill_points_on_success",
            |skill_points| {
                i32::try_from(skill_points)
                    .ok()
                    .map(SkillPoints::new)
                    .map(Modifier::ExtraSkillPointsOnSuccess)
            },
        ),
        RawBuiltin::new(
            "remaining_extra_quality_levels_on_success",
            Type::Integer,
            |outcome| {
                let remaining_extra_quality_levels_on_success = outcome
                    .remaining_modifiers
                    .count_of(Modifier::ExtraQualityLevelOnSuccess);

                Value::Integer(remaining_extra_quality_levels_on_success as i64)
            },
        ),
    ]
});

pub fn context_builtins() -> Vec<ContextBuiltin> {
    RAW_BUILTINS
        .iter()
        .enumerate()
        .map(|(declaration_id, raw_builtin)| raw_builtin.to_context_builtin(declaration_id))
        .collect()
}

pub fn runtime_builtins(outcome: &SkillCheckOutcome) -> Vec<RuntimeBuiltin> {
    RAW_BUILTINS
        .iter()
        .enumerate()
        .map(|(declaration_id, raw_builtin)| {
            raw_builtin.to_runtime_builtin(declaration_id, outcome)
        })
        .collect()
}
