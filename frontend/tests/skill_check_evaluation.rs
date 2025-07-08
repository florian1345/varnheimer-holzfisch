use dioxus_test_utils::event::{
    EventType,
    FocusEventType,
    FormEventType,
    MouseEventType,
    TestFormData,
};
use dioxus_test_utils::{Find, NodeRef, NodeRefAssertions, TestDom};
use frontend::App;
use kernal::prelude::*;

fn find_number_input(dom: &TestDom, index: usize) -> NodeRef<'_> {
    dom.find_all(".skill-check-state-form .number-input > input")[index]
}

fn enter_number(dom: &mut TestDom, index: usize, value: &str) {
    FormEventType::Input
        .at(find_number_input(&dom, index))
        .with(TestFormData {
            value: value.to_string(),
            ..Default::default()
        })
        .raise(dom);
    FocusEventType::Blur
        .at(find_number_input(&dom, index))
        .raise(dom);
}

fn enter_scholle(dom: &mut TestDom, code: &str) {
    FormEventType::Input
        .at(dom.find(".scholle-input textarea"))
        .with(TestFormData {
            value: code.to_string(),
            ..Default::default()
        })
        .raise(dom);
}

fn evaluate(dom: &mut TestDom) {
    MouseEventType::Click
        .at(dom.find(".evaluate-button"))
        .raise(dom);
}

fn assert_table_row(table_row: NodeRef<'_>, expected_values: impl IntoIterator<Item: AsRef<str>>) {
    let expected_values = expected_values.into_iter().collect::<Vec<_>>();
    let cells = table_row.find_all("td");

    assert_that!(&cells).has_length(expected_values.len());

    cells
        .into_iter()
        .zip(expected_values)
        .for_each(|(cell, expected_value)| {
            assert_that!(cell).contains_only_text(expected_value);
        })
}

#[test]
fn partial_no_modifiers() {
    let mut dom = TestDom::new(App);

    enter_number(&mut dom, 0, "15");
    enter_number(&mut dom, 1, "13");
    enter_number(&mut dom, 2, "12");
    enter_number(&mut dom, 3, "4");
    enter_scholle(
        &mut dom,
        "if is_critical_success then 2 * quality_level else quality_level",
    );
    evaluate(&mut dom);

    // Attributes 15, 13, 12 Skill Value 4
    //
    // * Critical Success QL 2: [1, 1, 1..=12], [1, 2..=13, 1], [2..=15, 1, 1]
    //  Probability 38 / 8000 ; Value 4
    // * Critical Success QL 1: [1, 1, 13..=20], [1, 14..=20, 1], [16..=20, 1, 1]
    //  Probability 20 / 8000 ; Value 2
    // * Success QL 2: [1..=15, 1..=13, 1..=12] - Critical Success QL 2
    //  Probability 2302 / 8000 ; Value 2
    // * Success QL 1:
    //  1..=15 x { [1..=13, 13..=16], [14, 1..=15], [15, 1..=14], [16, 1..=13], [17, 1..=12] },
    //  16 x { [1..=13, 1..=15], [14, 1..=14], [15, 1..=13], [16, 1..=12] },
    //  17 x { [1..=13, 1..=14], [14, 1..=13], [15, 1..=12] },
    //  18 x { [1..=13, 1..=13], [14, 1..=12] },
    //  [19, 1..=13, 1..=12] -
    //  ([1, 1, 13..=16], [1, 14..=17, 1], [16..=19, 1, 1])
    //  Probability: (1590 + 234 + 207 + 181 + 156 - 12) / 8000 = 2356 / 8000 ; Value 1
    // * Failure (Critical or ordinary): [1..=20, 1..=20, 1..=20] - Rest
    //  Probability: 3284 / 8000 ; Value 0

    let outcome_view = dom.find_last(".center-box").unwrap();
    let outcome_children = outcome_view.non_placeholder_children();

    assert_that!(&outcome_children).has_length(1);

    let probabilities_view = outcome_children[0];
    let probabilities_children = probabilities_view.children();

    assert_that!(&probabilities_children).has_length(2);

    let average_value_text = probabilities_children[0].as_text().unwrap();

    assert_that!(average_value_text).starts_with("Average value: 0.89");

    let table_rows = probabilities_children[1].find_all("tr");

    assert_table_row(table_rows[1], ["Failure", "41.05"]);
    assert_table_row(table_rows[2], ["QL 1", "29.70"]);
    assert_table_row(table_rows[3], ["QL 2", "29.25"]);
    assert_table_row(table_rows[4], ["QL 3", "0.00"]);
}

fn new_modifier_row(dom: &TestDom) -> NodeRef {
    dom.find_last(".modifier-row").unwrap()
}

fn select_modifier(dom: &mut TestDom, id: &str) {
    let select = new_modifier_row(dom).find("select");
    FormEventType::Change
        .at(select)
        .with(TestFormData {
            value: id.to_owned(),
            ..Default::default()
        })
        .raise(dom);
}

fn add_modifier(dom: &mut TestDom) {
    let add_button = new_modifier_row(dom).find(".end-button");
    MouseEventType::Click.at(add_button).raise(dom);
}

#[test]
fn with_roll_and_modifier() {
    let mut dom = TestDom::new(App);

    enter_number(&mut dom, 0, "15");
    enter_number(&mut dom, 1, "13");
    enter_number(&mut dom, 2, "12");
    enter_number(&mut dom, 3, "4");
    enter_number(&mut dom, 4, "14");
    enter_number(&mut dom, 5, "14");
    enter_number(&mut dom, 6, "18");
    enter_scholle(&mut dom, "if is_success then 1 else 0");

    select_modifier(&mut dom, "fate-point");
    add_modifier(&mut dom);

    evaluate(&mut dom);

    // Attributes 15, 13, 12 Skill Value 4 Roll 14, 14, 18
    // Rerolling 18 is best (75 % QL 1, 25 % Failure)

    let outcome_view = dom.find_last(".center-box").unwrap();
    let outcome_children = outcome_view.non_placeholder_children();

    assert_that!(&outcome_children).has_length(2);

    let recommendation = outcome_children[0];

    assert_that!(recommendation)
        .contains_only_text("Recommendation: Use fate point to reroll die 3");

    let probabilities_view = outcome_children[1];
    let probabilities_children = probabilities_view.children();
    let average_value_text = probabilities_children[0].as_text().unwrap();

    assert_that!(average_value_text).starts_with("Average value: 0.75");

    let table_rows = probabilities_children[1].find_all("tr");

    assert_table_row(table_rows[1], ["Failure", "25.00"]);
    assert_table_row(table_rows[2], ["QL 1", "75.00"]);
    assert_table_row(table_rows[3], ["QL 2", "0.00"]);
}

#[test]
fn with_runtime_error() {
    let mut dom = TestDom::new(App);

    enter_number(&mut dom, 0, "15");
    enter_number(&mut dom, 1, "13");
    enter_number(&mut dom, 2, "12");
    enter_number(&mut dom, 3, "4");
    enter_scholle(&mut dom, "100 / quality_level");
    evaluate(&mut dom);

    assert_that!(&dom.try_find("table")).is_none();
    assert_that!(dom.find(".error")).contains_only_text("Division by zero");
}
