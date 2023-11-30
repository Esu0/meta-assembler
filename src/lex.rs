//! 字句解析器

pub mod ast;
pub mod char_gen;
pub mod token_gen;

use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
enum _Error {
    /// オーバーフローを含む
    #[error("line {line}(column={column}): Token \"{token}\" cannot parse to integer.")]
    ParseIntError { line: usize, column: usize, token: String },
    #[error("line {line}(column={column}): encoding error.")]
    EncodeError { line: usize, column: usize },
}

#[derive(Debug, Error, Clone, PartialEq)]
#[error(transparent)]
pub struct Error {
    #[from]
    inner: _Error,
}

impl Error {
    fn new_parse_int_error(line: usize, column: usize, token: String) -> Self {
        Self {
            inner: _Error::ParseIntError { line, column, token },
        }
    }
    fn new_encode_error(line: usize, column: usize) -> Self {
        Self {
            inner: _Error::EncodeError { line, column },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn error_test() {
        let e = Error {
            inner: _Error::ParseIntError {
                line: 1,
                column: 1,
                token: "100a".to_owned(),
            },
        };
        assert_eq!(
            e.to_string(),
            "line 1(column=1): Token \"100a\" cannot parse to integer."
        );
    }
}
