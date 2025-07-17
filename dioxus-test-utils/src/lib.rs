pub mod event;
mod select;
pub mod signal;

use std::any::Any;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Formatter, Write};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use dioxus_core::{
    AttributeValue,
    ComponentFunction,
    Element,
    ElementId,
    Event,
    IntoAttributeValue,
    Template,
    TemplateAttribute,
    TemplateNode,
    VirtualDom,
    WriteMutations,
};
use dioxus_html::PlatformEventData;
use futures::FutureExt;
use kernal::prelude::*;
use kernal::{AssertThat, AssertThatData};
use slab::Slab;

use crate::event::{EventType, TestEvent, TestHtmlEventConverter};
pub use crate::select::Find;

#[derive(Clone, Copy)]
pub struct NodeRef<'dom> {
    id: NodeId,
    nodes: &'dom Nodes,
}

impl Debug for NodeRef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entry(&"id", &self.id)
            .entry(&"nodes", &self.nodes)
            .finish()
    }
}

fn expect_text(value: &AttributeValue) -> &str {
    let AttributeValue::Text(value) = value
    else {
        panic!("expected text attribute, but found {value:?}")
    };

    value.as_str()
}

fn write_attribute_value(html: &mut String, value: &AttributeValue) -> fmt::Result {
    match value {
        AttributeValue::Text(text) => write!(html, "\"{}\"", text.escape_debug()),
        AttributeValue::Float(f) => write!(html, "\"{f}\""),
        AttributeValue::Int(i) => write!(html, "\"{i}\""),
        AttributeValue::Bool(b) => write!(html, "\"{b}\""),
        AttributeValue::Listener(_) => write!(html, "[listener]"),
        AttributeValue::Any(_) => write!(html, "[any]"),
        AttributeValue::None => panic!("found none attribute value while generating HTML"),
    }
}

impl<'dom> NodeRef<'dom> {
    fn node(self) -> &'dom Node {
        &self.nodes[self.id]
    }

    fn kind(self) -> &'dom NodeKind {
        &self.node().kind
    }

    fn write_html(self, html: &mut String, indent: &mut String) -> fmt::Result {
        const INDENT_PER_LEVEL: &str = "  ";

        match self.kind() {
            NodeKind::Element {
                tag,
                attrs,
                children,
                ..
            } => {
                write!(html, "{indent}<{tag}")?;

                for (key, value) in attrs {
                    write!(html, " {}=", key.name)?;
                    write_attribute_value(html, value)?;
                }

                if children.is_empty() {
                    write!(html, "/>")
                }
                else {
                    writeln!(html, ">")?;

                    indent.push_str(INDENT_PER_LEVEL);

                    for &id in children {
                        let child = NodeRef { id, ..self };
                        child.write_html(html, indent)?;
                        writeln!(html)?;
                    }

                    indent.truncate(indent.len() - INDENT_PER_LEVEL.len());

                    write!(html, "{indent}</{tag}>")
                }
            },
            NodeKind::Text(text) => write!(
                html,
                "{indent}{text}",
                text = html_escape::encode_text(text)
            ),
            NodeKind::Placeholder => write!(html, "{indent}<!-- placeholder -->"),
        }
    }

    pub fn html(self) -> String {
        let mut html = String::new();
        self.write_html(&mut html, &mut String::new()).unwrap();
        html
    }

    pub fn is_element(self) -> bool {
        matches!(self.kind(), NodeKind::Element { .. })
    }

    pub fn is_placeholder(self) -> bool {
        matches!(self.kind(), NodeKind::Placeholder)
    }

    pub fn as_text(self) -> Option<&'dom str> {
        if let NodeKind::Text(text) = self.kind() {
            Some(text.as_str())
        }
        else {
            None
        }
    }

    pub fn tag(self) -> Option<&'static str> {
        if let NodeKind::Element { tag, .. } = self.kind() {
            Some(*tag)
        }
        else {
            None
        }
    }

    pub fn namespace(self) -> Option<&'static str> {
        if let &NodeKind::Element { namespace, .. } = self.kind() {
            namespace
        }
        else {
            None
        }
    }

    pub fn attributes(self) -> Vec<(AttributeKey, &'dom AttributeValue)> {
        if let NodeKind::Element { attrs, .. } = self.kind() {
            attrs.iter().map(|(k, v)| (*k, v)).collect()
        }
        else {
            Vec::new()
        }
    }

    pub fn classes(self) -> Vec<String> {
        self.attributes()
            .iter()
            .filter(|(key, _)| key.name == "class")
            .flat_map(|(_, value)| expect_text(value).split_whitespace().map(ToOwned::to_owned))
            .collect()
    }

    pub fn id(self) -> Option<&'dom str> {
        self.attributes()
            .into_iter()
            .find(|(key, _)| key.name == "id")
            .map(|(_, value)| expect_text(value))
    }

    fn children_iter(self) -> Option<impl Iterator<Item = NodeRef<'dom>>> {
        if let NodeKind::Element { children, .. } = self.kind() {
            Some(
                children
                    .iter()
                    .copied()
                    .map(move |id| NodeRef { id, ..self }),
            )
        }
        else {
            None
        }
    }

    pub fn children(self) -> Vec<NodeRef<'dom>> {
        self.children_iter()
            .map(Iterator::collect)
            .unwrap_or_default()
    }

    pub fn non_placeholder_children(self) -> Vec<NodeRef<'dom>> {
        self.children_iter()
            .map(|iter| iter.filter(|child| !child.is_placeholder()).collect())
            .unwrap_or_default()
    }

    pub fn text_children(self) -> Vec<&'dom str> {
        self.children()
            .into_iter()
            .filter_map(NodeRef::as_text)
            .collect()
    }

    pub fn parent(self) -> Option<NodeRef<'dom>> {
        self.node().parent.map(|id| NodeRef { id, ..self })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct NodeId(usize);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct AttributeKey {
    pub name: &'static str,
    pub namespace: Option<&'static str>,
}

#[derive(Clone, Debug)]
enum NodeKind {
    Element {
        tag: &'static str,
        namespace: Option<&'static str>,
        attrs: HashMap<AttributeKey, AttributeValue>,
        children: Vec<NodeId>,
    },
    Text(String),
    Placeholder,
}

#[derive(Clone, Debug)]
struct Node {
    kind: NodeKind,
    parent: Option<NodeId>,
}

impl Node {
    fn new(kind: NodeKind) -> Node {
        Node { kind, parent: None }
    }

    fn children(&self) -> &[NodeId] {
        match &self.kind {
            NodeKind::Element { children, .. } => children,
            _ => &[],
        }
    }

    fn children_mut(&mut self) -> &mut Vec<NodeId> {
        match &mut self.kind {
            NodeKind::Element { children, .. } => children,
            NodeKind::Text(_) => panic!("children of text node cannot be modified"),
            NodeKind::Placeholder => panic!("children of placeholder node cannot be modified"),
        }
    }
}

#[derive(Debug)]
struct Nodes {
    nodes: Slab<Node>,
    node_to_element_ids: HashMap<NodeId, ElementId>,
    element_to_node_ids: HashMap<ElementId, NodeId>,
}

impl Nodes {
    fn new() -> Nodes {
        Nodes {
            nodes: Slab::new(),
            node_to_element_ids: HashMap::new(),
            element_to_node_ids: HashMap::new(),
        }
    }

    fn get(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id.0)
    }

    fn get_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(id.0)
    }

    fn get_element_id(&self, id: NodeId) -> Option<ElementId> {
        self.node_to_element_ids.get(&id).cloned()
    }

    fn get_node_id(&self, id: ElementId) -> Option<NodeId> {
        self.element_to_node_ids.get(&id).copied()
    }

    fn get_element(&self, id: ElementId) -> Option<&Node> {
        self.get_node_id(id).and_then(|id| self.get(id))
    }

    fn get_element_mut(&mut self, id: ElementId) -> Option<&mut Node> {
        self.get_node_id(id).and_then(|id| self.get_mut(id))
    }

    fn insert(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.insert(node));

        for child in self.nodes[id.0].children().to_owned() {
            self.nodes[child.0].parent = Some(id);
        }

        id
    }

    fn assign(&mut self, node_id: NodeId, element_id: ElementId) {
        let replaced_element = self.node_to_element_ids.insert(node_id, element_id);
        let replaced_node = self.element_to_node_ids.insert(element_id, node_id);

        assert!(replaced_element.is_none());
        assert!(replaced_node.is_none());
    }

    fn remove(&mut self, id: NodeId) {
        if let Some(parent) = self[id].parent {
            self[parent]
                .children_mut()
                .retain(|&child_id| child_id != id);
        }

        self.nodes.remove(id.0);

        if let Some(element_id) = self.node_to_element_ids.remove(&id) {
            self.element_to_node_ids.remove(&element_id);
        }
    }
}

impl Index<NodeId> for Nodes {
    type Output = Node;

    fn index(&self, id: NodeId) -> &Node {
        self.get(id).expect("node does not exist")
    }
}

impl IndexMut<NodeId> for Nodes {
    fn index_mut(&mut self, id: NodeId) -> &mut Node {
        self.get_mut(id).expect("node does not exist")
    }
}

impl Index<ElementId> for Nodes {
    type Output = Node;

    fn index(&self, id: ElementId) -> &Node {
        self.get_element(id)
            .expect("element ID does not have assigned node ID")
    }
}

impl IndexMut<ElementId> for Nodes {
    fn index_mut(&mut self, id: ElementId) -> &mut Node {
        self.get_element_mut(id)
            .expect("element ID does not have assigned node ID")
    }
}

struct TestDomWriter {
    nodes: Nodes,
    stack: Vec<NodeId>,
    root_node_id: NodeId,
}

fn render_template_node(template_node: &TemplateNode, nodes: &mut Nodes) -> Node {
    use TemplateNode::*;
    match template_node {
        &Element {
            tag,
            namespace,
            attrs,
            children,
        } => {
            let attrs = attrs
                .iter()
                .filter_map(|attr| {
                    if let &TemplateAttribute::Static {
                        name,
                        namespace,
                        value,
                    } = attr
                    {
                        let key = AttributeKey { name, namespace };
                        Some((key, AttributeValue::Text(value.to_owned())))
                    }
                    else {
                        None
                    }
                })
                .collect();
            let children = children
                .iter()
                .map(|template_node| {
                    let node = render_template_node(template_node, nodes);
                    nodes.insert(node)
                })
                .collect();

            Node::new(NodeKind::Element {
                tag,
                namespace,
                attrs,
                children,
            })
        },
        Text { text } => Node::new(NodeKind::Text((*text).to_owned())),
        Dynamic { .. } => Node::new(NodeKind::Placeholder),
    }
}

impl TestDomWriter {
    fn new() -> TestDomWriter {
        let mut nodes = Nodes::new();

        let root_node_id = nodes.insert(Node::new(NodeKind::Element {
            tag: "dioxus-test-utils-root",
            namespace: None,
            attrs: HashMap::new(),
            children: vec![],
        }));
        nodes.assign(root_node_id, ElementId(0));

        TestDomWriter {
            nodes,
            stack: Vec::new(),
            root_node_id,
        }
    }

    fn load_path(&self, path: &[u8]) -> NodeId {
        let mut node = self.stack[self.stack.len() - 1];

        for &segment in path {
            node = self.nodes.get(node).unwrap().children()[segment as usize];
        }

        node
    }

    fn insert_children_in_parent(
        &mut self,
        reference_node: NodeId,
        children: Vec<NodeId>,
        offset: usize,
    ) {
        let parent_id = self
            .nodes
            .get(reference_node)
            .unwrap()
            .parent
            .expect("replacing element without parent");
        self.set_parent(parent_id, &children);
        let parent = self.nodes.get_mut(parent_id).unwrap();
        let child_idx = parent
            .children()
            .iter()
            .enumerate()
            .find(|(_, child)| **child == reference_node)
            .map(|(idx, _)| idx)
            .unwrap();
        let idx = child_idx + offset;

        parent.children_mut().splice(idx..idx, children);
    }

    fn set_parent(&mut self, parent_node: NodeId, child_nodes: &[NodeId]) {
        for &id in child_nodes {
            self.nodes[id].parent = Some(parent_node);
        }
    }

    fn replace(&mut self, replaced_node: NodeId, replacement_nodes: Vec<NodeId>) {
        if let Some(parent) = self.nodes[replaced_node].parent {
            self.set_parent(parent, &replacement_nodes);
        }

        self.insert_children_in_parent(replaced_node, replacement_nodes, 0);
        self.nodes.remove(replaced_node);
    }

    fn pop_many(&mut self, count: usize) -> Vec<NodeId> {
        self.stack.split_off(self.stack.len() - count)
    }
}

impl WriteMutations for TestDomWriter {
    fn append_children(&mut self, id: ElementId, m: usize) {
        let id = self
            .nodes
            .get_node_id(id)
            .expect("appending children to non-existing node");
        let mut to_append = self.pop_many(m);

        self.set_parent(id, &to_append);
        self.nodes[id].children_mut().append(&mut to_append);
    }

    fn assign_node_id(&mut self, path: &'static [u8], id: ElementId) {
        self.nodes.assign(self.load_path(path), id);
    }

    fn create_placeholder(&mut self, id: ElementId) {
        let node_id = self.nodes.insert(Node::new(NodeKind::Placeholder));
        self.nodes.assign(node_id, id);
        self.stack.push(node_id);
    }

    fn create_text_node(&mut self, value: &str, id: ElementId) {
        let node_id = self
            .nodes
            .insert(Node::new(NodeKind::Text(value.to_owned())));
        self.nodes.assign(node_id, id);
        self.stack.push(node_id);
    }

    fn load_template(&mut self, template: Template, index: usize, id: ElementId) {
        // TODO dioxus-web does some caching here, maybe we can as well?

        let template_root = render_template_node(&template.roots[index], &mut self.nodes);
        let node_id = self.nodes.insert(template_root);
        self.nodes.assign(node_id, id);
        self.stack.push(node_id);
    }

    fn replace_node_with(&mut self, id: ElementId, m: usize) {
        let replaced_node = self
            .nodes
            .get_node_id(id)
            .expect("replacing non-existing element");
        let replacement_nodes = self.pop_many(m);
        self.replace(replaced_node, replacement_nodes);
    }

    fn replace_placeholder_with_nodes(&mut self, path: &'static [u8], m: usize) {
        let replacement_nodes = self.pop_many(m);
        let replaced_node = self.load_path(path);
        self.replace(replaced_node, replacement_nodes);
    }

    fn insert_nodes_after(&mut self, id: ElementId, m: usize) {
        let anchor_node = self
            .nodes
            .get_node_id(id)
            .expect("inserting after non-existing element");
        let inserted_nodes = self.pop_many(m);

        self.insert_children_in_parent(anchor_node, inserted_nodes, 1);
    }

    fn insert_nodes_before(&mut self, id: ElementId, m: usize) {
        let anchor_node = self
            .nodes
            .get_node_id(id)
            .expect("inserting before non-existing element");
        let inserted_nodes = self.pop_many(m);

        self.insert_children_in_parent(anchor_node, inserted_nodes, 0);
    }

    fn set_attribute(
        &mut self,
        name: &'static str,
        ns: Option<&'static str>,
        value: &AttributeValue,
        id: ElementId,
    ) {
        match &mut self.nodes[id].kind {
            NodeKind::Element { attrs, .. } => {
                let key = AttributeKey {
                    name,
                    namespace: ns,
                };

                if let AttributeValue::None = value {
                    attrs.remove(&key);
                }
                else {
                    attrs.insert(key, value.clone());
                }
            },
            _ => panic!("cannot set attribute of non-element node"),
        }
    }

    fn set_node_text(&mut self, value: &str, id: ElementId) {
        if let NodeKind::Text(text) = &mut self.nodes[id].kind {
            *text = value.to_owned();
            return;
        }

        // TODO is the rest actually relevant?

        for child_id in self.nodes[id].children().to_owned() {
            self.nodes.remove(child_id);
        }

        let new_text_node_id = self
            .nodes
            .insert(Node::new(NodeKind::Text(value.to_owned())));

        self.nodes[id].children_mut().push(new_text_node_id);
    }

    fn create_event_listener(&mut self, _: &'static str, _: ElementId) {}

    fn remove_event_listener(&mut self, _: &'static str, _: ElementId) {}

    fn remove_node(&mut self, id: ElementId) {
        let removed_node = self
            .nodes
            .get_node_id(id)
            .expect("removing non-existing element");
        self.nodes.remove(removed_node);
    }

    fn push_root(&mut self, id: ElementId) {
        let node_id = self
            .nodes
            .get_node_id(id)
            .expect("pushing non-existing element");
        self.stack.push(node_id);
    }
}

pub struct TestDom {
    virtual_dom: VirtualDom,
    writer: TestDomWriter,
}

impl TestDom {
    fn new_with_virtual_dom(mut virtual_dom: VirtualDom) -> TestDom {
        dioxus_html::events::set_event_converter(Box::new(TestHtmlEventConverter));

        let mut writer = TestDomWriter::new();
        virtual_dom.rebuild(&mut writer);

        let mut dom = TestDom {
            virtual_dom,
            writer,
        };

        dom.update();
        dom
    }

    pub fn new(root: fn() -> Element) -> TestDom {
        TestDom::new_with_virtual_dom(VirtualDom::new(root))
    }

    pub fn new_with_props<P: Clone + 'static, M: 'static>(
        root: impl ComponentFunction<P, M>,
        root_props: P,
    ) -> TestDom {
        TestDom::new_with_virtual_dom(VirtualDom::new_with_props(root, root_props))
    }

    fn update(&mut self) {
        while self.virtual_dom.wait_for_work().now_or_never().is_some() {
            self.virtual_dom.render_immediate(&mut self.writer);
        }
    }

    pub fn raise(&mut self, event: TestEvent<impl EventType>) {
        let platform_event_data = PlatformEventData::new(Box::new(event.data));
        let dioxus_event = Event::new(Rc::new(platform_event_data) as Rc<dyn Any>, true);
        let element_id = self
            .writer
            .nodes
            .get_element_id(event.node_id)
            .expect("cannot handle event for node without element ID");
        self.virtual_dom
            .runtime()
            .handle_event(event.event_type.name(), dioxus_event, element_id);
        self.update();
    }

    pub fn root_node(&self) -> NodeRef<'_> {
        NodeRef {
            id: self.writer.root_node_id,
            nodes: &self.writer.nodes,
        }
    }
}

pub trait NodeRefAssertions {
    #[allow(clippy::wrong_self_convention)] // for assertion chaining
    fn is_text(self, expected_text: impl AsRef<str>) -> Self;

    fn has_tag(self, tag: impl AsRef<str>) -> Self;

    fn has_exactly_classes(self, classes: impl IntoIterator<Item = impl AsRef<str>>) -> Self;

    fn contains_only_text(self, expected_text: impl AsRef<str>) -> Self;

    fn has_attribute(self, name: impl AsRef<str>, value: impl IntoAttributeValue) -> Self;

    fn has_value(self, value: impl IntoAttributeValue) -> Self;
}

impl<'dom, N: Borrow<NodeRef<'dom>>> NodeRefAssertions for AssertThat<N> {
    fn is_text(self, expected_text: impl AsRef<str>) -> Self {
        let actual_text = self
            .data()
            .borrow()
            .as_text()
            .expect("asserted node is not a text node");
        assert_that!(actual_text).is_equal_to(expected_text.as_ref());
        self
    }

    fn has_tag(self, tag: impl AsRef<str>) -> Self {
        assert_that!(self.data().borrow().tag()).contains(tag.as_ref());
        self
    }

    fn has_exactly_classes(self, classes: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        assert_that!(self.data().borrow().classes()).contains_exactly_in_any_order(
            classes.into_iter().map(|class| class.as_ref().to_owned()),
        );
        self
    }

    fn contains_only_text(self, expected_text: impl AsRef<str>) -> Self {
        let children = self.data().borrow().non_placeholder_children();

        assert_that!(&children).has_length(1);
        assert_that!(&children[0]).is_text(expected_text);

        self
    }

    fn has_attribute(self, name: impl AsRef<str>, value: impl IntoAttributeValue) -> Self {
        let node = *self.data().borrow();
        let expected_name = name.as_ref();
        let expected_value = value.into_value();

        assert_that!(node.attributes()).contains_elements_matching(|(key, value)| {
            let key_matches = key.name == expected_name && key.namespace.is_none();
            let value_matches = *value == &expected_value;

            key_matches && value_matches
        });

        self
    }

    fn has_value(self, value: impl IntoAttributeValue) -> Self {
        self.has_attribute("value", value)
    }
}
