use std::cell::RefCell;
use std::fs::{File, OpenOptions};
use std::io::{Read, Result as IoResult, Write};
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

    pub fn open_write(path: &str) -> IoResult<Self> {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        Ok(Self {
            inner: Rc::new(RefCell::new(Some(Inner(file)))),
        })
    }

    pub fn open_append(path: &str) -> IoResult<Self> {
        let file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)?;
        Ok(Self {
            inner: Rc::new(RefCell::new(Some(Inner(file)))),
        })
    }

    pub fn open_read_write(path: &str) -> IoResult<Self> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)?;
        Ok(Self {
            inner: Rc::new(RefCell::new(Some(Inner(file)))),
        })
    }

    pub fn close(&self) {
        *self.inner.borrow_mut() = None;
    }

    pub fn is_open(&self) -> bool {
        self.inner.borrow().is_some()
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

    pub fn write_str(&self, content: &str) -> IoResult<()> {
        let mut guard = self.inner.borrow_mut();
        if let Some(file) = guard.as_mut() {
            file.0.write_all(content.as_bytes())?;
            Ok(())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "file is closed",
            ))
        }
    }

    pub fn write_line(&self, content: &str) -> IoResult<()> {
        let mut buf = String::from(content);
        buf.push('\n');
        self.write_str(&buf)
    }

    pub fn flush(&self) -> IoResult<()> {
        let mut guard = self.inner.borrow_mut();
        if let Some(file) = guard.as_mut() {
            file.0.flush()?;
            Ok(())
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
