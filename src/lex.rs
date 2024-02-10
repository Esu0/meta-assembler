//! 字句解析器

pub mod char_gen;
pub mod token_gen;
pub mod word_gen;

use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// オーバーフローを含む
    #[error("\"{token}\"は{radix}進数の整数として解釈できません。")]
    ParseIntError { token: Box<str>, radix: u32 },
    #[error("整数の接頭辞\"{found}\"は不正です。")]
    IntPrefixError { found: Box<str> },
    #[error("encoding error.")]
    EncodeError,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
#[error(transparent)]
pub struct Error {
    #[from]
    inner: ErrorKind,
}

impl Error {
    const fn new_parse_int_error(token: Box<str>, radix: u32) -> Self {
        Self {
            inner: ErrorKind::ParseIntError { token, radix },
        }
    }

    pub const fn encode_error() -> Self {
        Self {
            inner: ErrorKind::EncodeError,
        }
    }

    const fn new_int_prefix_error(found: Box<str>) -> Self {
        Self {
            inner: ErrorKind::IntPrefixError { found },
        }
    }
}

impl From<char_gen::EncodeError> for Error {
    fn from(_: char_gen::EncodeError) -> Self {
        Self::encode_error()
    }
}
