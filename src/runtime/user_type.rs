use super::Handle;
use ustr::Ustr;

#[derive(Clone, Debug)]
pub enum UserTy {
    Handle(Handle),
}

impl UserTy {
    pub fn tag(&self) -> Ustr {
        match self {
            UserTy::Handle(handle) => handle.tag(),
        }
    }
}

impl PartialEq for UserTy {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UserTy::Handle(a), UserTy::Handle(b)) => a == b,
        }
    }
}

impl Eq for UserTy {}

impl From<Handle> for UserTy {
    fn from(handle: Handle) -> Self {
        UserTy::Handle(handle)
    }
}
