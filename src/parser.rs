//! 構文解析

pub mod assembly;
pub mod cgr;

use std::{fmt::Display, io, error};

use thiserror::Error;

use crate::lex::{token_gen::Token, char_gen::EncodeError};

#[derive(Error, Debug)]
pub enum ErrorKind {
    #[error("予期せぬトークン{found}が検出されました。トークン{expected}が予測されます。")]
    UnexpectedToken { expected: Box<str>, found: Box<str> },
    #[error("数値が範囲外です。範囲は{min}から{max}ですが、{found}が検出されました。")]
    NumOutOfRange { min: u64, max: u64, found: u64 },
    #[error("設定{found}は存在しません。")]
    NonExsistentProperty { found: Box<str> },
    #[error("設定{found}は既に存在します。")]
    AlreadyExsistentProperty { found: Box<str> },
    #[error("テーブルの数が最大数を超えました。テーブルは最大63個です。")]
    TooManyTables,
    #[error("ファイルの終端に到達しました。")]
    UnexpectedEof,
    #[error("テーブル{found}が定義されていません。")]
    TableNotFound{ found: Box<str> },
    #[error(transparent)]
    EncodeError(#[from] EncodeError),
    #[error(transparent)]
    LexicalError(#[from] crate::lex::Error),
}

impl ErrorKind {
    pub fn unexpected_token(expected: String, found: Token) -> Self {
        Self::UnexpectedToken {
            expected: expected.into_boxed_str(),
            found: String::from(found).into_boxed_str(),
        }
    }

    pub fn num_out_of_range(min: u64, max: u64, found: u64) -> Self {
        Self::NumOutOfRange { min, max, found }
    }
}

#[derive(Error, Debug)]
#[error("line {line}(column: {column}): {kind}")]
struct ErrorSimple {
    kind: ErrorKind,
    line: usize,
    column: usize,
}

#[derive(Error, Debug)]
enum ErrorInner {
    #[error(transparent)]
    Simple(#[from] ErrorSimple),
    #[error(transparent)]
    Custom(Box<dyn std::error::Error + Sync + Send>),
}

#[derive(Error, Debug)]
#[error(transparent)]
pub struct Error {
    #[from]
    inner: ErrorInner,
}

// TokenにDisplayを実装したら消す
fn token_to_string(token: Token) -> String {
    match token {
        Token::Integer(n) => n.to_string(),
        Token::Opr(s) => s,
        Token::Word(s) => s,
        #[allow(unreachable_patterns)]
        _ => unimplemented!(),
    }
}

impl Error {
    pub fn kind(&self) -> Option<&ErrorKind> {
        match self.inner {
            ErrorInner::Simple(ref e) => Some(&e.kind),
            ErrorInner::Custom(_) => None,
        }
    }

    pub fn new(kind: ErrorKind, line: usize, column: usize) -> Self {
        Self {
            inner: ErrorInner::Simple(ErrorSimple { kind, line, column }),
        }
    }

    pub fn unexpected_token(expected: String, found: Token, position: (usize, usize)) -> Self {
        Self::new(
            ErrorKind::UnexpectedToken {
                expected: expected.into(),
                found: token_to_string(found).into(),
            },
            position.0,
            position.1,
        )
    }

    pub fn unexpected_eof(position: (usize, usize)) -> Self {
        Self::new(ErrorKind::UnexpectedEof, position.0, position.1)
    }
}

impl From<crate::lex::Error> for Error {
    fn from(value: crate::lex::Error) -> Self {
        Self {
            inner: ErrorInner::Custom(Box::new(value)),
        }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self {
            inner: ErrorInner::Custom(Box::new(value)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReaderPos(usize, usize);

impl Display for ReaderPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}({})", self.0, self.1)
    }
}
