use std::iter;

use dioxus::prelude::*;
use model::check::outcome::SkillCheckOutcomeProbabilities;
use model::evaluation::Evaluated;
use model::probability::Probability;
use model::skill::QualityLevel;

fn all_quality_levels_with_none() -> impl Iterator<Item = Option<QualityLevel>> {
    iter::once(None).chain(QualityLevel::ALL.into_iter().map(Some))
}

fn format_optional_quality_level(ql: Option<QualityLevel>) -> String {
    match ql {
        Some(ql) => format!("QL {}", ql.as_u8()),
        None => "Failure".to_owned(),
    }
}

fn outcome_probability(
    probabilities: &SkillCheckOutcomeProbabilities,
    quality_level: Option<QualityLevel>,
) -> Probability {
    let mut probability = Probability::ZERO;

    probabilities
        .outcomes()
        .filter(|(outcome, _)| outcome.quality_level() == quality_level)
        .map(|(_, probability)| probability)
        .for_each(|p| probability += p);

    probability
}

#[component]
fn ProbabilitiesView(probabilities: SkillCheckOutcomeProbabilities) -> Element {
    rsx! {
        table {
            tr {
                th { "Outcome" }
                th { "Probability (%)" }
            }

            for quality_level in all_quality_levels_with_none() {
                tr {
                    td {
                        { format_optional_quality_level(quality_level) }
                    }

                    td {
                        {
                            let probability = outcome_probability(&probabilities, quality_level);
                            format!("{:.02}", probability.as_f64() * 100.0)
                        }
                    }
                }
            }
        }
    }
}

#[component]
pub fn EvaluatedProbabilitiesView(
    evaluated_probabilities: Evaluated<SkillCheckOutcomeProbabilities>,
) -> Element {
    rsx! {
        div {
            class: "info",

            { format!("Average value: {}", evaluated_probabilities.evaluation.as_f64()) }

            ProbabilitiesView {
                probabilities: evaluated_probabilities.evaluated.clone(),
            }
        }
    }
}
