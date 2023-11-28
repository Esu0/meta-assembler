//! 字句解析器

pub mod ast;
pub mod char_gen;
pub mod token_gen;

use thiserror::Error;

#[derive(Debug, Error)]
enum _Error {
    /// オーバーフローを含む
    #[error("line {line}: Token \"{token}\" cannot parse to integer.")]
    ParseIntError {
        line: usize,
        token: String,
    },
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct Error {
    #[from]
    inner: _Error,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn error_test() {
        let e = Error {
            inner: _Error::ParseIntError { line: 1, token: "100a".to_owned() },
        };
        assert_eq!(e.to_string(), "line 1: Token \"100a\" cannot parse to integer.");
    }
}