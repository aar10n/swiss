use super::NameError;

use crate::id::{module_id, ModuleId};
use crate::source::{source_id, SourceFile, SourceId, SourceSpan, Spanned};

use either::{Either, Left, Right};
use smallvec::{smallvec, Array, SmallVec};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use ustr::Ustr;

pub type PathTreeIndex = usize;

// MARK: PathTree

pub struct PathTree<T> {
    root: Rc<RefCell<TreeNode>>,
    items: Vec<T>,
}

impl<T: From<(usize, Ustr)>> PathTree<T> {
    pub fn new() -> Self {
        let root = TreeNode::new_parent("".into());
        Self {
            root: Rc::new(RefCell::new(root)),
            items: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, path: impl PathLike) -> Result<(&T, PathTreeIndex), Spanned<String>> {
        let node = self.get_node(path.clone())?;
        let node = node.borrow();
        if let Some(item_index) = node.index {
            Ok((&self.items[item_index], item_index))
        } else {
            Err(path.to_spanned_string())
        }
    }

    pub fn get_mut(
        &mut self,
        path: impl PathLike,
    ) -> Result<(&mut T, PathTreeIndex), Spanned<String>> {
        let node = self.get_node(path.clone())?;
        let node = node.borrow();
        if let Some(item_index) = node.index {
            Ok((&mut self.items[item_index], item_index))
        } else {
            Err(path.to_spanned_string())
        }
    }

    pub fn get_or_insert(
        &mut self,
        path: impl PathLike,
    ) -> Result<(&mut T, PathTreeIndex), Spanned<String>> {
        let node = self.get_or_insert_node(path.clone())?;
        let node = node.borrow();
        if let Some(item_index) = node.index {
            Ok((&mut self.items[item_index], item_index))
        } else {
            Err(path.to_spanned_string())
        }
    }

    pub fn insert(&mut self, path: impl PathLike) -> Result<&mut T, Spanned<String>> {
        let node = self.insert_node(path.clone())?;
        let node = node.borrow();
        if let Some(item_index) = node.index {
            Ok(&mut self.items[item_index])
        } else {
            Err(path.to_spanned_string())
        }
    }

    // Operation Method Impls

    fn get_node(&self, path: impl PathLike) -> Result<Rc<RefCell<TreeNode>>, Spanned<String>> {
        let mut current = self.root.clone();
        for name in path.parts() {
            let next = if let Some(next) = current.borrow().get_child(&name.raw) {
                next
            } else {
                return Err(name.to_string_inner());
            };
            current = next;
        }

        Ok(current)
    }

    fn get_or_insert_node(
        &mut self,
        path: impl PathLike,
    ) -> Result<Rc<RefCell<TreeNode>>, Spanned<String>> {
        let mut parent = self.root.clone();
        for name in path.dir_parts() {
            let next = {
                let current = parent.borrow_mut();
                current.get_child(&name.raw)
            };

            if let Some(next) = next {
                parent = next;
            } else {
                return Err(name.to_string_inner());
            }
        }

        let mut parent = parent.borrow_mut();
        let name = path.base_part();
        if let Some(next) = parent.get_child_mut(&name.raw) {
            Ok(next)
        } else {
            let index = self.items.len();
            let item = T::from((index, name.raw));
            self.items.push(item);
            let node = parent.add_child(name.raw, index);
            Ok(node)
        }
    }

    fn insert_node(
        &mut self,
        path: impl PathLike,
    ) -> Result<Rc<RefCell<TreeNode>>, Spanned<String>> {
        let mut parent = self.root.clone();
        for name in path.dir_parts() {
            let next = {
                let current = parent.borrow_mut();
                current.get_child(&name.raw)
            };

            if let Some(next) = next {
                parent = next;
            } else {
                return Err(name.to_string_inner());
            }
        }

        let mut parent = parent.borrow_mut();
        let name = path.base_part();
        if parent.get_child(&name).is_some() {
            return Err(name.to_string_inner());
        }

        let index = self.items.len();
        let item = T::from((index, name.raw));
        self.items.push(item);
        let node = parent.add_child(name.raw, index);
        Ok(node)
    }
}

impl<T> Index<PathTreeIndex> for PathTree<T> {
    type Output = T;

    fn index(&self, index: PathTreeIndex) -> &Self::Output {
        &self.items[index]
    }
}

impl<T> IndexMut<PathTreeIndex> for PathTree<T> {
    fn index_mut(&mut self, index: PathTreeIndex) -> &mut Self::Output {
        &mut self.items[index]
    }
}

// MARK: TreeNode

struct TreeNode {
    name: Ustr,
    index: Option<usize>,
    children: Vec<(Ustr, Rc<RefCell<TreeNode>>)>,
}

impl TreeNode {
    pub fn new_child(name: Ustr, index: usize) -> Self {
        Self {
            name,
            index: Some(index),
            children: Vec::new(),
        }
    }

    pub fn new_parent(name: Ustr) -> Self {
        Self {
            name,
            index: None,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, name: Ustr, index: usize) -> Rc<RefCell<TreeNode>> {
        let node = Self::new_child(name, index);
        let child = Rc::new(RefCell::new(node));
        self.children.push((name, child.clone()));
        child
    }

    pub fn get_child(&self, name: &Ustr) -> Option<Rc<RefCell<TreeNode>>> {
        self.children
            .iter()
            .find(|(child_name, _)| child_name == name)
            .map(|(_, child)| child.clone())
    }

    pub fn get_child_mut(&mut self, name: &Ustr) -> Option<Rc<RefCell<TreeNode>>> {
        self.children
            .iter()
            .find(|(child_name, _)| child_name == name)
            .map(|(_, child)| child.clone())
    }
}

// MARK: PathLike

pub trait PathLike: Sized + Clone {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn parts(&self) -> SmallVec<[Spanned<Ustr>; 4]>;
    fn dir_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]>;
    fn base_part(&self) -> Spanned<Ustr>;

    fn to_spanned_ustr(&self) -> Spanned<Ustr>;
    fn to_spanned_string(&self) -> Spanned<String> {
        self.to_spanned_ustr().to_string_inner()
    }
}

impl PathLike for &str {
    fn len(&self) -> usize {
        self.split("::").count()
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.split("::")
            .map(|part| Spanned::from(Ustr::from(part)))
            .collect()
    }
    fn dir_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        SmallVec::new()
    }
    fn base_part(&self) -> Spanned<Ustr> {
        Spanned::from(Ustr::from(self))
    }
    fn to_spanned_ustr(&self) -> Spanned<Ustr> {
        Spanned::from(Ustr::from(self))
    }
}

impl PathLike for Spanned<Ustr> {
    fn len(&self) -> usize {
        self.raw.split("::").count()
    }
    fn is_empty(&self) -> bool {
        false
    }
    fn parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        smallvec![self.clone()]
    }
    fn dir_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        smallvec![]
    }
    fn base_part(&self) -> Spanned<Ustr> {
        self.clone()
    }
    fn to_spanned_ustr(&self) -> Spanned<Ustr> {
        self.clone()
    }
}

impl<A> PathLike for SmallVec<A>
where
    A: Array<Item = Spanned<Ustr>>,
{
    fn len(&self) -> usize {
        self.len()
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.clone().into_iter().collect()
    }
    fn dir_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.iter().cloned().take(self.len() - 1).collect()
    }
    fn base_part(&self) -> Spanned<Ustr> {
        self.last().unwrap().clone()
    }
    fn to_spanned_ustr(&self) -> Spanned<Ustr> {
        join_path(self.clone().into_iter()).to_ustr_inner()
    }
}

impl PathLike for Vec<Spanned<Ustr>> {
    fn len(&self) -> usize {
        self.len()
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.clone().into_iter().collect()
    }
    fn dir_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.iter().cloned().take(self.len() - 1).collect()
    }
    fn base_part(&self) -> Spanned<Ustr> {
        self.last().unwrap().clone()
    }
    fn to_spanned_ustr(&self) -> Spanned<Ustr> {
        join_path(self.clone().into_iter()).to_ustr_inner()
    }
}

//

fn join_path(parts: impl Iterator<Item = Spanned<Ustr>>) -> Spanned<String> {
    let (string, spans) = parts
        .map(|part| (part.raw.to_string(), part.span))
        .unzip::<String, SourceSpan, Vec<String>, Vec<SourceSpan>>();

    let (source_id, start) = spans
        .first()
        .map(|span| (span.source_id, span.start))
        .unwrap();
    let end = spans.last().map(|span| span.end).unwrap();
    Spanned::new(string.join("::"), SourceSpan::new(source_id, start, end))
}
