use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum IoSink {
    Stdout,
    Buffer(String),
}

#[derive(Clone, Debug)]
pub struct IoHandle {
    inner: Rc<RefCell<IoSink>>,
}

impl IoHandle {
    pub fn stdout() -> Self {
        IoHandle {
            inner: Rc::new(RefCell::new(IoSink::Stdout)),
        }
    }

    pub fn buffer() -> Self {
        IoHandle {
            inner: Rc::new(RefCell::new(IoSink::Buffer(String::new()))),
        }
    }

    pub fn write_str(&self, s: &str) -> std::io::Result<()> {
        match &mut *self.inner.borrow_mut() {
            IoSink::Stdout => {
                let mut stdout = std::io::stdout();
                stdout.write_all(s.as_bytes())?;
                stdout.flush()
            }
            IoSink::Buffer(buf) => {
                buf.push_str(s);
                Ok(())
            }
        }
    }

    pub fn take_buffer(&self) -> Option<String> {
        match &mut *self.inner.borrow_mut() {
            IoSink::Buffer(buf) => Some(std::mem::take(buf)),
            IoSink::Stdout => None,
        }
    }
}
