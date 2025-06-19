use std::fmt;
use std::fmt::Write;
use std::ops::Deref;

use cssparser::{ParserInput, ToCss};
use precomputed_hash::PrecomputedHash;
use selectors::attr::{AttrSelectorOperation, CaseSensitivity, NamespaceConstraint};
use selectors::bloom::BloomFilter;
use selectors::context::{
    MatchingContext,
    MatchingForInvalidation,
    MatchingMode,
    NeedsSelectorFlags,
    QuirksMode,
};
use selectors::matching::ElementSelectorFlags;
use selectors::parser::{NonTSPseudoClass, Selector, SelectorParseErrorKind};
use selectors::{Element, OpaqueElement, SelectorImpl, matching, parser};
use string_cache::{Atom, EmptyStaticAtomSet};

use crate::{ElementWrapper, NodeWrapper};

#[derive(Clone, Default, Eq, PartialEq)]
pub struct CssName(Atom<EmptyStaticAtomSet>);

impl AsRef<str> for CssName {
    fn as_ref(&self) -> &str {
        self.0.deref()
    }
}

impl ToCss for CssName {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: Write,
    {
        write!(dest, "{}", &self.0)
    }
}

impl<'str> From<&'str str> for CssName {
    fn from(value: &'str str) -> Self {
        CssName(Atom::from(value))
    }
}

impl PrecomputedHash for CssName {
    fn precomputed_hash(&self) -> u32 {
        self.0.precomputed_hash()
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct PseudoClass;

impl ToCss for PseudoClass {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: Write,
    {
        dest.write_str("")
    }
}

impl NonTSPseudoClass for PseudoClass {
    type Impl = ElementWrapperSelectorImpl;

    fn is_active_or_hover(&self) -> bool {
        false
    }

    fn is_user_action_state(&self) -> bool {
        false
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct PseudoElement;

impl ToCss for PseudoElement {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: Write,
    {
        dest.write_str("")
    }
}

impl parser::PseudoElement for PseudoElement {
    type Impl = ElementWrapperSelectorImpl;
}

#[derive(Clone, Copy, Debug)]
pub struct ElementWrapperSelectorImpl;

impl SelectorImpl for ElementWrapperSelectorImpl {
    type ExtraMatchingData<'a> = ();
    type AttrValue = CssName;
    type Identifier = CssName;
    type LocalName = CssName;
    type NamespaceUrl = CssName;
    type NamespacePrefix = CssName;
    type BorrowedNamespaceUrl = CssName;
    type BorrowedLocalName = CssName;
    type NonTSPseudoClass = PseudoClass;
    type PseudoElement = PseudoElement;
}

// Another wrapper around ElementWrapper to avoid confusing methods from Element being available on
// ElementWrapper.
#[derive(Clone, Debug)]
struct SelectorElementWrapper<'dom>(ElementWrapper<'dom>);

impl<'dom> Element for SelectorElementWrapper<'dom> {
    type Impl = ElementWrapperSelectorImpl;

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self.0.template_node)
    }

    fn parent_element(&self) -> Option<SelectorElementWrapper<'dom>> {
        self.0
            .parent
            .as_ref()
            .map(|parent| SelectorElementWrapper((**parent).clone()))
    }

    fn parent_node_is_shadow_root(&self) -> bool {
        false
    }

    fn containing_shadow_host(&self) -> Option<SelectorElementWrapper<'dom>> {
        None
    }

    fn is_pseudo_element(&self) -> bool {
        false
    }

    // TODO generating the children again for each query seems expensive?

    fn prev_sibling_element(&self) -> Option<SelectorElementWrapper<'dom>> {
        self.0
            .parent
            .as_ref()
            .and_then(|parent| {
                parent
                    .children()
                    .into_iter()
                    .take(self.0.index_in_parent)
                    .rev()
                    .find_map(|node| node.as_element())
            })
            .map(SelectorElementWrapper)
    }

    fn next_sibling_element(&self) -> Option<SelectorElementWrapper<'dom>> {
        self.0
            .parent
            .as_ref()
            .and_then(|parent| {
                parent
                    .children()
                    .into_iter()
                    .skip(self.0.index_in_parent + 1)
                    .find_map(|node| node.as_element())
            })
            .map(SelectorElementWrapper)
    }

    fn first_element_child(&self) -> Option<SelectorElementWrapper<'dom>> {
        self.0
            .children()
            .into_iter()
            .find_map(NodeWrapper::as_element)
            .map(SelectorElementWrapper)
    }

    fn is_html_element_in_html_document(&self) -> bool {
        false
    }

    fn has_local_name(&self, local_name: &CssName) -> bool {
        self.0.tag == local_name.as_ref()
    }

    fn has_namespace(&self, _: &CssName) -> bool {
        false
    }

    fn is_same_type(&self, other: &SelectorElementWrapper<'dom>) -> bool {
        self.0.tag == other.0.tag
    }

    fn attr_matches(
        &self,
        _: &NamespaceConstraint<&CssName>,
        local_name: &CssName,
        operation: &AttrSelectorOperation<&CssName>,
    ) -> bool {
        self.0
            .attributes()
            .into_iter()
            .find(|attribute| attribute.name == local_name.as_ref())
            .is_some_and(|attribute| operation.eval_str(&attribute.value))
    }

    fn match_non_ts_pseudo_class(
        &self,
        _: &PseudoClass,
        _: &mut MatchingContext<ElementWrapperSelectorImpl>,
    ) -> bool {
        false
    }

    fn match_pseudo_element(
        &self,
        _: &PseudoElement,
        _: &mut MatchingContext<ElementWrapperSelectorImpl>,
    ) -> bool {
        false
    }

    fn apply_selector_flags(&self, _: ElementSelectorFlags) {}

    fn is_link(&self) -> bool {
        self.0.tag == "link"
    }

    fn is_html_slot_element(&self) -> bool {
        true
    }

    fn has_id(&self, id: &CssName, case_sensitivity: CaseSensitivity) -> bool {
        self.0.id().is_some_and(|actual_id| {
            case_sensitivity.eq(actual_id.as_bytes(), id.as_ref().as_bytes())
        })
    }

    fn has_class(&self, name: &CssName, case_sensitivity: CaseSensitivity) -> bool {
        self.0
            .classes()
            .into_iter()
            .any(|class| case_sensitivity.eq(class.as_bytes(), name.as_ref().as_bytes()))
    }

    fn has_custom_state(&self, _: &CssName) -> bool {
        false
    }

    fn imported_part(&self, _: &CssName) -> Option<CssName> {
        None
    }

    fn is_part(&self, _: &CssName) -> bool {
        false
    }

    fn is_empty(&self) -> bool {
        self.0
            .children()
            .into_iter()
            .all(|child| child.as_text().is_some_and(str::is_empty))
    }

    fn is_root(&self) -> bool {
        self.0.parent.is_none()
    }

    fn add_element_unique_hashes(&self, _: &mut BloomFilter) -> bool {
        false
    }
}

#[derive(Clone, Copy, Debug)]
struct Parser;

impl<'i> parser::Parser<'i> for Parser {
    type Impl = ElementWrapperSelectorImpl;
    type Error = SelectorParseErrorKind<'i>;
}

fn parse_selector(selector: &str) -> Selector<ElementWrapperSelectorImpl> {
    let mut parser_input = ParserInput::new(selector);
    let mut parser = cssparser::Parser::new(&mut parser_input);

    Selector::parse(&Parser, &mut parser).unwrap()
}

impl<'dom> ElementWrapper<'dom> {
    pub fn find_all(&self, selector: &str) -> Vec<ElementWrapper<'dom>> {
        let selector = parse_selector(selector);
        let mut caches = Default::default();
        let mut context = MatchingContext::new(
            MatchingMode::Normal,
            None,
            &mut caches,
            QuirksMode::NoQuirks,
            NeedsSelectorFlags::No,
            MatchingForInvalidation::No,
        );

        let mut remaining = vec![self.clone()];
        let mut matches = Vec::new();

        while let Some(element) = remaining.pop() {
            remaining.extend(
                element
                    .children()
                    .into_iter()
                    .rev()
                    .filter_map(NodeWrapper::as_element),
            );
            let element = SelectorElementWrapper(element);

            if matching::matches_selector(&selector, 0, None, &element, &mut context) {
                matches.push(element.0);
            }
        }

        matches
    }

    pub fn try_find(&self, selector: &str) -> Option<ElementWrapper<'dom>> {
        let all_matches = self.find_all(selector);

        assert!(
            all_matches.len() <= 1,
            "expected selector `{}` to find at most 1 match, but it found {}",
            selector,
            all_matches.len()
        );

        all_matches.into_iter().next()
    }

    pub fn find(&self, selector: &str) -> ElementWrapper<'dom> {
        let all_matches = self.find_all(selector);

        assert_eq!(
            all_matches.len(),
            1,
            "expected selector `{}` to find exactly 1 match, but it found {}",
            selector,
            all_matches.len()
        );

        all_matches.into_iter().next().unwrap()
    }

    pub fn find_first(&self, selector: &str) -> Option<ElementWrapper<'dom>> {
        self.find_all(selector).into_iter().next()
    }
}
