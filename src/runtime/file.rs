use std::cell::RefCell;
use std::fs::File;
use std::io::{Read, Result as IoResult};
use std::rc::Rc;

#[derive(Debug)]
struct Inner(File);

#[derive(Clone, Debug)]
pub struct FileHandle {
    inner: Rc<RefCell<Option<Inner>>>,
}

impl FileHandle {
    pub fn open_read(path: &str) -> IoResult<Self> {
        let file = File::open(path)?;
        Ok(Self {
            inner: Rc::new(RefCell::new(Some(Inner(file)))),
        })
    }

    pub fn close(&self) {
        *self.inner.borrow_mut() = None;
    }

    pub fn read_all(&self) -> IoResult<String> {
        let mut guard = self.inner.borrow_mut();
        if let Some(file) = guard.as_mut() {
            let mut buf = String::new();
            file.0.read_to_string(&mut buf)?;
            Ok(buf)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "file is closed",
            ))
        }
    }
}

impl Drop for Inner {
    fn drop(&mut self) {
        // File closes on drop automatically; explicit Drop impl is here to make the intent clear.
    }
}
