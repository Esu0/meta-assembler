//! 字句解析器

pub mod ast;
pub mod char_gen;
pub mod token_gen;

use thiserror::Error;

#[derive(Debug, Error)]
enum Error {
    /// オーバーフローを含む
    #[error("line {line}: Token \"{token}\" cannot parse to integer.")]
    ParseIntError {
        line: usize,
        token: String,
    },
}