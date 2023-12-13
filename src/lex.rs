//! 字句解析器

pub mod ast;
pub mod char_gen;
pub mod token_gen;
pub mod word_gen;

use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// オーバーフローを含む
    #[error("Token \"{token}\" cannot parse to integer.")]
    ParseIntError { token: String },
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
    fn new_parse_int_error(_: usize, _: usize, token: String) -> Self {
        Self {
            inner: ErrorKind::ParseIntError { token },
        }
    }
    fn new_encode_error(_: usize, _: usize) -> Self {
        Self {
            inner: ErrorKind::EncodeError,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn error_test() {
        let e = Error {
            inner: ErrorKind::ParseIntError {
                token: "100a".to_owned(),
            },
        };
        assert_eq!(e.to_string(), "Token \"100a\" cannot parse to integer.");
    }
}
