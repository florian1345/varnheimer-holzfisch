use std::hint::black_box;
use std::time::Duration;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use model::check::PartialSkillCheckState;
use model::check::modifier::{Aptitude, Modifier, ModifierState};
use model::engine::VarnheimerHolzfischEngine;
use model::skill::{Attribute, SkillPoints};
use scholle::ScholleEvaluator;

fn criterion_benchmark(c: &mut Criterion) {
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
    let states = [
        (no_modifiers, "no modifiers"),
        (inaptitude, "inaptitude"),
        (aptitude, "aptitude"),
        (aptitude_2_dice, "aptitude 2 dice"),
        (fate_point, "fate point"),
    ];

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
    let evaluators = [
        (max_quality_level_evaluator, "max quality level evaluator"),
        (
            max_success_probability_evaluator,
            "max success probability evaluator",
        ),
        (complex_evaluator, "complex evaluator"),
    ];

    for (state, name) in states {
        let mut g = c.benchmark_group(name);
        let g = g.sample_size(10).measurement_time(Duration::from_secs(30));

        for (evaluator, name) in evaluators.clone() {
            let engine = VarnheimerHolzfischEngine { evaluator };
            g.bench_function(name, |b| {
                b.iter_batched(
                    || state.clone(),
                    |state| engine.evaluate_partial(black_box(state)),
                    BatchSize::SmallInput,
                )
            });
        }
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
