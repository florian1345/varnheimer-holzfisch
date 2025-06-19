mod select;

use std::borrow::Borrow;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use dioxus_core::{
    Attribute,
    AttributeValue,
    ComponentFunction,
    DynamicNode,
    NoOpMutations,
    ScopeId,
    TemplateAttribute,
    TemplateNode,
    VNode,
    VirtualDom,
};
use futures::FutureExt;
use kernal::prelude::*;
use kernal::{AssertThat, AssertThatData};

pub struct AttributeWrapper {
    name: &'static str,
    value: String,
}

impl From<Attribute> for AttributeWrapper {
    fn from(attribute: Attribute) -> AttributeWrapper {
        let value = match attribute.value {
            AttributeValue::Text(s) => s,
            AttributeValue::Float(f) => f.to_string(),
            AttributeValue::Int(i) => i.to_string(),
            AttributeValue::Bool(b) => b.to_string(),
            _ => panic!(
                "attribute value {:?} cannot be represented as a string",
                attribute.value
            ),
        };

        AttributeWrapper {
            name: attribute.name,
            value,
        }
    }
}

#[derive(Clone)]
pub struct ElementWrapper<'dom> {
    template_node: &'dom TemplateNode,
    parent: Option<Rc<ElementWrapper<'dom>>>,
    index_in_parent: usize,
    tag: &'static str,
    attrs: &'static [TemplateAttribute],
    children: &'static [TemplateNode],
    vnode: &'dom VNode,
    virtual_dom: &'dom VirtualDom,
}

impl<'dom> ElementWrapper<'dom> {
    pub fn tag(&self) -> &'static str {
        self.tag
    }

    pub fn attributes(&self) -> Vec<AttributeWrapper> {
        let mut attributes = Vec::new();

        for attribute in self.attrs {
            match *attribute {
                TemplateAttribute::Static { name, value, .. } => {
                    attributes.push(AttributeWrapper {
                        name,
                        value: value.to_owned(),
                    })
                },
                TemplateAttribute::Dynamic { id } => attributes.extend(
                    self.vnode.dynamic_attrs[id]
                        .iter()
                        .cloned()
                        .map(AttributeWrapper::from),
                ),
            }
        }

        attributes
    }

    pub fn classes(&self) -> Vec<String> {
        self.attributes()
            .iter()
            .filter(|attr| attr.name == "class")
            .flat_map(|attr| attr.value.split_whitespace().map(ToOwned::to_owned))
            .collect()
    }

    pub fn id(&self) -> Option<String> {
        self.attributes()
            .into_iter()
            .find(|attr| attr.name == "id")
            .map(|attr| attr.value)
    }

    pub fn children(&self) -> Vec<NodeWrapper<'dom>> {
        let mut output = Vec::new();

        for child_template_node in self.children {
            template_node_children_rec(
                child_template_node,
                self.vnode,
                self.virtual_dom,
                Some(self),
                &mut output,
            );
        }

        output
    }
}

impl<'dom> Debug for ElementWrapper<'dom> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ElementWrapper {{ tag: {:?}, attrs: {:?}, children: {:?}, vnode: {:?} }}",
            self.tag, self.attrs, self.children, self.vnode
        )
    }
}

#[derive(Clone, Debug)]
pub enum NodeWrapper<'dom> {
    Element(ElementWrapper<'dom>),
    Text(&'dom str),
}

impl<'dom> NodeWrapper<'dom> {
    pub fn as_element(self) -> Option<ElementWrapper<'dom>> {
        match self {
            NodeWrapper::Element(element) => Some(element),
            NodeWrapper::Text(_) => None,
        }
    }

    pub fn expect_element(self) -> ElementWrapper<'dom> {
        self.as_element()
            .expect("expected element node but found text")
    }

    pub fn as_text(self) -> Option<&'dom str> {
        match self {
            NodeWrapper::Text(text) => Some(text),
            NodeWrapper::Element(_) => None,
        }
    }

    pub fn expect_text(self) -> &'dom str {
        self.as_text()
            .expect("expected text node but found element")
    }
}

fn template_node_children_rec<'dom>(
    template_node: &'dom TemplateNode,
    vnode: &'dom VNode,
    virtual_dom: &'dom VirtualDom,
    parent: Option<&ElementWrapper<'dom>>,
    output: &mut Vec<NodeWrapper<'dom>>,
) {
    match *template_node {
        TemplateNode::Element {
            tag,
            attrs,
            children,
            ..
        } => {
            let index = output.len();

            output.push(NodeWrapper::Element(ElementWrapper {
                template_node,
                parent: parent.map(|parent| Rc::new(parent.clone())),
                index_in_parent: index,
                tag,
                attrs,
                children,
                vnode,
                virtual_dom,
            }))
        },
        TemplateNode::Text { text } => output.push(NodeWrapper::Text(text)),
        TemplateNode::Dynamic { id } => match &vnode.dynamic_nodes[id] {
            DynamicNode::Text(text) => output.push(NodeWrapper::Text(&text.value)),
            DynamicNode::Fragment(vnodes) => {
                vnodes
                    .iter()
                    .for_each(|vnode| children_rec(vnode, virtual_dom, parent, output));
            },
            DynamicNode::Component(component) => {
                let scope = component
                    .mounted_scope(id, vnode, virtual_dom)
                    .expect("component not mounted");
                children_rec(scope.root_node(), virtual_dom, parent, output);
            },
            _ => {},
        },
    }
}

fn children_rec<'dom>(
    vnode: &'dom VNode,
    virtual_dom: &'dom VirtualDom,
    parent: Option<&ElementWrapper<'dom>>,
    output: &mut Vec<NodeWrapper<'dom>>,
) {
    for template_node in vnode.template.roots {
        template_node_children_rec(template_node, vnode, virtual_dom, parent, output);
    }
}

pub struct VirtualDomWrapper {
    virtual_dom: VirtualDom,
}

impl VirtualDomWrapper {
    pub fn new_with_props<P: Clone + 'static, M: 'static>(
        root: impl ComponentFunction<P, M>,
        root_props: P,
    ) -> VirtualDomWrapper {
        let mut virtual_dom = VirtualDom::new_with_props(root, root_props);
        virtual_dom.rebuild_in_place();

        while virtual_dom.wait_for_work().now_or_never().is_some() {
            virtual_dom.render_immediate(&mut NoOpMutations);
        }

        VirtualDomWrapper { virtual_dom }
    }

    pub fn root_nodes(&self) -> Vec<NodeWrapper<'_>> {
        let root_vnode = self
            .virtual_dom
            .get_scope(ScopeId::APP)
            .unwrap()
            .root_node();

        let mut root_nodes = Vec::new();
        children_rec(root_vnode, &self.virtual_dom, None, &mut root_nodes);
        root_nodes
    }
}

pub trait ElementWrapperAssertions {
    fn has_tag(self, tag: impl AsRef<str>) -> Self;

    fn has_exactly_classes(self, classes: impl IntoIterator<Item = impl AsRef<str>>) -> Self;
}

impl<'dom, E: Borrow<ElementWrapper<'dom>>> ElementWrapperAssertions for AssertThat<E> {
    fn has_tag(self, tag: impl AsRef<str>) -> Self {
        assert_that!(self.data().borrow().tag()).is_equal_to(tag.as_ref());
        self
    }

    fn has_exactly_classes(self, classes: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        assert_that!(self.data().borrow().classes()).contains_exactly_in_any_order(
            classes.into_iter().map(|class| class.as_ref().to_owned()),
        );
        self
    }
}

pub trait NodeWrapperAssertions {
    #[allow(clippy::wrong_self_convention)] // for assertion chaining
    fn is_text(self, expected_text: impl AsRef<str>) -> Self;
}

impl<'dom, N: Borrow<NodeWrapper<'dom>>> NodeWrapperAssertions for AssertThat<N> {
    fn is_text(self, expected_text: impl AsRef<str>) -> Self {
        let actual_text = self.data().borrow().clone().expect_text();
        assert_that!(actual_text).is_equal_to(expected_text.as_ref());
        self
    }
}
