//! 字句解析器

pub mod char_gen;
pub mod token_gen;
pub mod word_gen;

use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// オーバーフローを含む
    #[error("Token \"{token}\" cannot parse to integer.")]
    ParseIntError { token: Box<str> },
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
    const fn new_parse_int_error(token: Box<str>) -> Self {
        Self {
            inner: ErrorKind::ParseIntError { token },
        }
    }

    const fn encode_error() -> Self {
        Self {
            inner: ErrorKind::EncodeError,
        }
    }
}

impl From<char_gen::EncodeError> for Error {
    fn from(_: char_gen::EncodeError) -> Self {
        Self::encode_error()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn error_test() {
        let e = Error {
            inner: ErrorKind::ParseIntError {
                token: "100a".into(),
            },
        };
        assert_eq!(e.to_string(), "Token \"100a\" cannot parse to integer.");
    }
}
