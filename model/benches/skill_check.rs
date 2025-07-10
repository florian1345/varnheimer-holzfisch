use std::hint::black_box;
use std::time::Duration;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use model::check::modifier::{Aptitude, Modifier, ModifierState};
use model::check::{PartialSkillCheckState, SkillCheckState};
use model::engine::VarnheimerHolzfischEngine;
use model::roll::Roll;
use model::skill::{Attribute, SkillPoints};
use scholle::ScholleEvaluator;

fn evaluators() -> Vec<(ScholleEvaluator, &'static str)> {
    let max_quality_level_evaluator = ScholleEvaluator::new("quality_level").unwrap();
    let max_success_probability_evaluator =
        ScholleEvaluator::new("if is_success then 1 else 0").unwrap();
    let complex_evaluator = ScholleEvaluator::new(
        r#"
        let
            critical_success_bonus = if is_critical_success then 1.5 else 0.0,
            critical_failure_penalty = if is_critical_failure then 1.5 else 0.0,
            remaining_fate_point_value = as_float(remaining_fate_points) * 0.8
        in
            (
                if is_success
                then
                    as_float(quality_level) + critical_success_bonus
                else
                    -1.0 - critical_failure_penalty
            ) + remaining_fate_point_value
    "#,
    )
    .unwrap();

    vec![
        (max_quality_level_evaluator, "max quality level evaluator"),
        (
            max_success_probability_evaluator,
            "max success probability evaluator",
        ),
        (complex_evaluator, "complex evaluator"),
    ]
}

fn run_benchmark_kind<T: Clone, R>(
    states: Vec<(T, &'static str)>,
    mut evaluate: impl FnMut(&VarnheimerHolzfischEngine<ScholleEvaluator>, T) -> R,
    group_prefix: &'static str,
    c: &mut Criterion,
) {
    for (state, name) in states {
        let mut g = c.benchmark_group(format!("{group_prefix} > {name}"));
        let g = g.sample_size(10).measurement_time(Duration::from_secs(30));

        for (evaluator, name) in evaluators() {
            let engine = VarnheimerHolzfischEngine { evaluator };
            g.bench_function(name, |b| {
                b.iter_batched(
                    || state.clone(),
                    |state| evaluate(&engine, black_box(state)),
                    BatchSize::SmallInput,
                )
            });
        }
    }
}

fn run_benchmark_partial(c: &mut Criterion) {
    let default = PartialSkillCheckState {
        attributes: [Attribute::new(1), Attribute::new(1), Attribute::new(1)],
        roll_caps: [None, None, None],
        fixed_rolls: [None, None, None],
        skill_value: SkillPoints::new(0),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: Default::default(),
        inaptitude: false,
    };
    let no_modifiers = PartialSkillCheckState {
        attributes: [Attribute::new(14), Attribute::new(13), Attribute::new(11)],
        skill_value: SkillPoints::new(5),
        ..default.clone()
    };
    let inaptitude = PartialSkillCheckState {
        attributes: [Attribute::new(13), Attribute::new(12), Attribute::new(13)],
        skill_value: SkillPoints::new(6),
        inaptitude: true,
        ..default.clone()
    };
    let aptitude = PartialSkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(15), Attribute::new(14)],
        skill_value: SkillPoints::new(8),
        modifiers: ModifierState::from_modifiers([Modifier::Aptitude(Aptitude::new(1).unwrap())]),
        ..default.clone()
    };
    let aptitude_2_dice = PartialSkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(11), Attribute::new(10)],
        skill_value: SkillPoints::new(9),
        modifiers: ModifierState::from_modifiers([Modifier::Aptitude(Aptitude::new(2).unwrap())]),
        ..default.clone()
    };
    let fate_point = PartialSkillCheckState {
        attributes: [Attribute::new(7), Attribute::new(9), Attribute::new(9)],
        skill_value: SkillPoints::new(11),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        ..default.clone()
    };
    let states = vec![
        (no_modifiers, "no modifiers"),
        (inaptitude, "inaptitude"),
        (aptitude, "aptitude"),
        (aptitude_2_dice, "aptitude 2 dice"),
        (fate_point, "fate point"),
    ];

    run_benchmark_kind(
        states,
        VarnheimerHolzfischEngine::evaluate_partial,
        "partial",
        c,
    );
}

fn roll(value: u8) -> Roll {
    Roll::new(value).unwrap()
}

fn run_benchmark_complete(c: &mut Criterion) {
    let default = SkillCheckState {
        attributes: [Attribute::new(1), Attribute::new(1), Attribute::new(1)],
        rolls: [roll(1), roll(1), roll(1)],
        skill_value: SkillPoints::new(0),
        extra_quality_levels_on_success: None,
        extra_skill_points_on_success: SkillPoints::new(0),
        modifiers: Default::default(),
    };
    let fate_point_failure = SkillCheckState {
        attributes: [Attribute::new(14), Attribute::new(13), Attribute::new(13)],
        rolls: [roll(18), roll(14), roll(17)],
        skill_value: SkillPoints::new(6),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        ..default.clone()
    };
    let fate_point_success = SkillCheckState {
        attributes: [Attribute::new(13), Attribute::new(11), Attribute::new(10)],
        rolls: [roll(14), roll(11), roll(3)],
        skill_value: SkillPoints::new(9),
        modifiers: ModifierState::from_modifiers([Modifier::FatePoint]),
        ..default.clone()
    };
    let fate_point_and_aptitude = SkillCheckState {
        attributes: [Attribute::new(7), Attribute::new(9), Attribute::new(9)],
        rolls: [roll(11), roll(20), roll(14)],
        skill_value: SkillPoints::new(13),
        modifiers: ModifierState::from_modifiers([
            Modifier::FatePoint,
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
        ]),
        ..default.clone()
    };
    let aptitude_and_all_kinds_of_extras = SkillCheckState {
        attributes: [Attribute::new(12), Attribute::new(9), Attribute::new(10)],
        rolls: [roll(14), roll(11), roll(18)],
        skill_value: SkillPoints::new(9),
        modifiers: ModifierState::from_modifiers([
            Modifier::Aptitude(Aptitude::new(1).unwrap()),
            Modifier::ExtraQualityLevelOnSuccess,
            Modifier::ExtraSkillPointsOnSuccess(SkillPoints::new(2)),
            Modifier::ExtraSkillPoints(SkillPoints::new(1)),
        ]),
        ..default.clone()
    };
    let states = vec![
        (fate_point_failure, "fate point failure"),
        (fate_point_success, "fate point success"),
        (fate_point_and_aptitude, "fate point and aptitude"),
        (
            aptitude_and_all_kinds_of_extras,
            "aptitude and all kinds of extras",
        ),
    ];

    run_benchmark_kind(
        states,
        VarnheimerHolzfischEngine::evaluate_all_actions,
        "complete",
        c,
    );
}

fn run_all_benchmarks(c: &mut Criterion) {
    run_benchmark_partial(c);
    run_benchmark_complete(c);
}

criterion_group!(benches, run_all_benchmarks);
criterion_main!(benches);
