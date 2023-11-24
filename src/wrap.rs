//! 色々迷走して没になったライブラリ
//! 
//! OptionとかResultみたいなものを抽象化したもの
use std::fmt;

pub trait WrappedOrError<Item>: Sized {
    type Error;
    fn unwrap_or_else(self, f: impl FnOnce(Self::Error) -> Item) -> Item;

    fn error_msg(err: &Self::Error) -> Option<&str>;

    fn unwrap(self) -> Item {
        self.expect("Called `WrappedOrError::unwrap()` on a `None` value")
    }

    fn expect(self, msg: &str) -> Item {
        #[cold]
        #[inline(never)]
        fn panic_with_msg(msg: &str, s: Option<&str>) -> ! {
            match s {
                Some(s) => panic!("{msg}: {s}"),
                None => panic!("{msg}"),
            }
        }
        self.unwrap_or_else(|s| panic_with_msg(msg, Self::error_msg(&s)))
    }
}

pub trait Wrapped<Item>: Sized {
    fn unwrap_or_else(self, f: impl FnOnce() -> Item) -> Item;

    fn unwrap(self) -> Item {
        self.expect("Called `Wrapped::unwrap()` on a `None` value")
    }

    fn expect(self, msg: &str) -> Item {
        #[cold]
        #[inline(never)]
        fn panic_with_msg(msg: &str) -> ! {
            panic!("{msg}")
        }
        self.unwrap_or_else(|| panic_with_msg(msg))
    }
}

impl<T> Wrapped<T> for Option<T> {
    fn unwrap_or_else(self, f: impl FnOnce() -> T) -> T {
        self.unwrap_or_else(f)
    }
}

impl<T, E: fmt::Debug> WrappedOrError<T> for Result<T, E> {
    type Error = E;
    fn unwrap_or_else(self, f: impl FnOnce(Self::Error) -> T) -> T {
        self.unwrap_or_else(f)
    }

    fn error_msg(err: &Self::Error) -> Option<&str> {
        Some(format!("{:?}", err).leak())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wrap<T>(T);


impl<T> From<T> for Wrap<T> {
    fn from(t: T) -> Self {
        Wrap(t)
    }
}

impl<T> Wrapped<T> for Wrap<T> {
    fn unwrap_or_else(self, _f: impl FnOnce() -> T) -> T {
        self.0
    }

    fn unwrap(self) -> T {
        self.0
    }

    fn expect(self, _msg: &str) -> T {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WrapError<T>(T);

impl<T> From<T> for WrapError<T> {
    fn from(t: T) -> Self {
        WrapError(t)
    }
}

impl<T> WrappedOrError<T> for WrapError<T> {
    type Error = ();
    fn unwrap_or_else(self, _f: impl FnOnce(Self::Error) -> T) -> T {
        self.0
    }

    fn error_msg(_err: &Self::Error) -> Option<&str> {
        None
    }

    fn unwrap(self) -> T {
        self.0
    }

    fn expect(self, _msg: &str) -> T {
        self.0
    }
}
