#[macro_export]
macro_rules! unique_id_type {
    ($vis:vis $ty:ident) => {
        paste::paste! {
            $vis mod [<$ty:snake>] {
                use static_init::dynamic;
                use std::sync::atomic::{AtomicUsize, Ordering};

                #[dynamic]
                static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

                pub fn next() -> super::$ty {
                    let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
                    super::$ty::new(id)
                }
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            $vis struct $ty {
                _raw: usize,
            }

            impl $ty {
                pub const INVALID: $ty = $ty::new(usize::MAX);

                const fn new(id: usize) -> $ty {
                    $ty { _raw: id }
                }

                $vis fn raw(&self) -> usize {
                    self._raw
                }

                $vis fn is_invalid(&self) -> bool {
                    self == &Self::INVALID
                }
            }
        }
    };
}

unique_id_type!(pub NodeId);
unique_id_type!(pub SourceId);
unique_id_type!(pub ModuleId);
