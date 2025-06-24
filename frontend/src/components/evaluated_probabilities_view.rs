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

#[cfg(test)]
mod tests {
    use dioxus_test_utils::{Find, NodeRefAssertions, TestDom};
    use kernal::prelude::*;
    use model::check::modifier::Modifier;
    use model::check::outcome::SkillCheckOutcome;
    use model::check::outcome::SkillCheckOutcomeKind::*;
    use model::evaluation::Evaluation;
    use rstest::rstest;

    use super::*;

    fn mount(evaluated_probabilities: Evaluated<SkillCheckOutcomeProbabilities>) -> TestDom {
        TestDom::new_with_props(
            EvaluatedProbabilitiesView,
            EvaluatedProbabilitiesViewProps {
                evaluated_probabilities,
            },
        )
    }

    fn skill_check_outcome_probabilities(
        outcome_probabilities: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
    ) -> SkillCheckOutcomeProbabilities {
        outcome_probabilities
            .into_iter()
            .map(|(outcome, probability)| {
                SkillCheckOutcomeProbabilities::of_known_outcome(outcome) * probability
            })
            .reduce(|mut lhs, rhs| {
                lhs.saturating_add_assign(&rhs);
                lhs
            })
            .unwrap_or_else(SkillCheckOutcomeProbabilities::default)
    }

    #[test]
    fn displays_value() {
        let dom = mount(Evaluated {
            evaluated: skill_check_outcome_probabilities([]),
            evaluation: Evaluation::new(0.25).unwrap(),
        });

        assert_that!(dom.find("div.info").text_children()[0]).is_equal_to("Average value: 0.25");
    }

    fn prob(value: f64) -> Probability {
        Probability::new(value).unwrap()
    }

    #[rstest]
    #[case::all_zero([], [ "0.00", "0.00", "0.00", "0.00", "0.00", "0.00", "0.00" ])]
    #[case::one_outcome_per_row(
        [
            (Failure.without_modifiers(), prob(0.1)),
            (Success(QualityLevel::ONE).without_modifiers(), prob(0.2)),
            (CriticalSuccess(QualityLevel::TWO).without_modifiers(), prob(0.3)),
            (SpectacularSuccess(QualityLevel::THREE).without_modifiers(), prob(0.4)),
            (Success(QualityLevel::FOUR).with_modifiers([Modifier::FatePoint]), prob(0.5)),
            (Success(QualityLevel::FIVE).without_modifiers(), prob(0.6)),
            (Success(QualityLevel::SIX).without_modifiers(), prob(0.7)),
        ],
        [ "10.00", "20.00", "30.00", "40.00", "50.00", "60.00", "70.00" ]
    )]
    #[case::multiple_outcomes_per_row(
        [
            (Failure.without_modifiers(), prob(0.0001)),
            (CriticalFailure.without_modifiers(), prob(0.0002)),
            (SpectacularFailure.without_modifiers(), prob(0.0003)),
            (Failure.with_modifiers([Modifier::FatePoint]), prob(0.0005)),
        ],
        [ "0.11", "0.00", "0.00", "0.00", "0.00", "0.00", "0.00" ]
    )]
    fn displays_table(
        #[case] outcome_probabilities: impl IntoIterator<Item = (SkillCheckOutcome, Probability)>,
        #[case] expected_displayed_probabilities: [&'static str; 7],
    ) {
        const EXPECTED_OUTCOMES: [&str; 7] =
            ["Failure", "QL 1", "QL 2", "QL 3", "QL 4", "QL 5", "QL 6"];

        let dom = mount(Evaluated {
            evaluated: skill_check_outcome_probabilities(outcome_probabilities),
            evaluation: Evaluation::new(0.25).unwrap(),
        });

        let header_cells = dom.find_all("table > tr > th");

        assert_that!(&header_cells[0]).contains_only_text("Outcome");
        assert_that!(&header_cells[1]).contains_only_text("Probability (%)");

        let non_header_rows = dom
            .find_all("table > tr")
            .into_iter()
            .skip(1)
            .collect::<Vec<_>>();

        assert_that!(&non_header_rows).has_length(7);

        let expected_rows = EXPECTED_OUTCOMES
            .into_iter()
            .zip(expected_displayed_probabilities);

        for ((expected_outcome, expected_probability), row) in expected_rows.zip(non_header_rows) {
            let cells = row.find_all("td");

            assert_that!(&cells).has_length(2);
            assert_that!(&cells[0]).contains_only_text(expected_outcome);
            assert_that!(&cells[1]).contains_only_text(expected_probability);
        }
    }
}
