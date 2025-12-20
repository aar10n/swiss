use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::env;

use ustr::Ustr;

use crate::runtime::value::VRef;
use crate::runtime::{Context, Value};

thread_local! {
    // Global registry of heap nodes that can participate in cycles.
    static REGISTRY: RefCell<Registry> = RefCell::new(Registry::new());
}

#[derive(Clone)]
enum HeapEntry {
    List(std::rc::Weak<std::cell::RefCell<Vec<Value>>>),
    Object(std::rc::Weak<std::cell::RefCell<Vec<(Ustr, Value)>>>),
}

// MARK: Registry

struct Registry {
    entries: HashMap<usize, HeapEntry>,
}

impl Registry {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn register(&mut self, ptr: usize, entry: HeapEntry) {
        self.entries.insert(ptr, entry);
    }

    fn unregister(&mut self, ptr: usize) {
        self.entries.remove(&ptr);
    }
}

pub fn register_list(buf: &VRef<Vec<Value>>) {
    REGISTRY.with(|reg| {
        reg.borrow_mut()
            .register(buf.ptr(), HeapEntry::List(buf.downgrade()));
    });
}

pub fn register_object(buf: &VRef<Vec<(Ustr, Value)>>) {
    REGISTRY.with(|reg| {
        reg.borrow_mut()
            .register(buf.ptr(), HeapEntry::Object(buf.downgrade()));
    });
}

pub fn collect_cycles(ctx: &Context) {
    REGISTRY.with(|reg| {
        let mut registry = reg.borrow_mut();
        if registry.entries.is_empty() {
            return;
        }

        let trace_on = env::var("TRACE_COLLECTOR").is_ok();
        let mut marked: HashSet<usize> = HashSet::new();
        let mut work: VecDeque<Value> = VecDeque::new();

        // Roots: local scopes, last_value, and module constants.
        if trace_on {
            eprintln!(
                "[TRACE_COLLECTOR] scopes={} last_value={}",
                ctx.local_scopes().len(),
                ctx.last_value.is_some()
            );
            for (idx, scope) in ctx.local_scopes().iter().enumerate() {
                for (name, _) in scope.vars().iter() {
                    eprintln!("[TRACE_COLLECTOR] scope{} name={}", idx, name);
                }
            }
        }
        for scope in ctx.local_scopes() {
            for value in scope.vars().values() {
                work.push_back(value.get());
            }
        }
        if let Some(v) = ctx.last_value.clone() {
            work.push_back(v);
        }
        for module in ctx.modules.iter() {
            for constant in module.names.iter_constants() {
                work.push_back(constant.value.get());
            }
        }

        let root_count = work.len();
        while let Some(v) = work.pop_front() {
            match v {
                Value::List(list) => {
                    let ptr = list.buf_ptr();
                    if marked.insert(ptr) {
                        for child in list.borrow_slice().iter() {
                            work.push_back(child.clone());
                        }
                    }
                }
                Value::Object(obj) => {
                    let ptr = obj.ptr();
                    if marked.insert(ptr) {
                        for (_, val) in obj.borrow().iter() {
                            work.push_back(val.clone());
                        }
                    }
                }
                Value::Tuple(items) => {
                    for item in items.iter() {
                        work.push_back((**item).clone());
                    }
                }
                Value::Ref(r) => {
                    work.push_back(r.get());
                }
                _ => {}
            }
        }

        let mut to_remove = Vec::new();
        let mut swept = 0usize;
        for (ptr, entry) in registry.entries.iter() {
            if marked.contains(ptr) {
                continue;
            }

            match entry {
                HeapEntry::List(weak) => {
                    if let Some(rc) = weak.upgrade() {
                        rc.borrow_mut().clear();
                    }
                }
                HeapEntry::Object(weak) => {
                    if let Some(rc) = weak.upgrade() {
                        rc.borrow_mut().clear();
                    }
                }
            }
            to_remove.push(*ptr);
        }

        for ptr in to_remove {
            registry.unregister(ptr);
            swept += 1;
        }

        if trace_on {
            eprintln!(
                "[TRACE_COLLECTOR] roots={} marked={} swept={}",
                root_count,
                marked.len(),
                swept
            );
        }
    });
}
