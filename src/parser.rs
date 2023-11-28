//! 構文解析

pub mod assembly;
pub mod cgr;

use std::io;

use thiserror::Error;

use crate::lex::token_gen::TokenGenerator;

#[derive(Error, Debug)]
pub enum ErrorKind {
    #[error("予期せぬトークン{found}が検出されました。トークン{expected}が予測されます。")]
    UnexpectedToken {expected: String, found: String},
    #[error("ファイルの終端に到達しました。")]
    UnexpectedEof,
}

#[derive(Error, Debug)]
#[error("line {line}({column}): {kind}")]
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
    inner: ErrorInner,
}

impl Error {
    pub fn kind(&self) -> Option<&ErrorKind> {
        match self.inner {
            ErrorInner::Simple(ref e) => Some(&e.kind),
            ErrorInner::Custom(_) => None,
        }
    }

    pub fn new(kind: ErrorKind, position: (usize, usize)) -> Self {
        Self {
            inner: ErrorInner::Simple(ErrorSimple { kind, line: position.0, column: position.1 })
        }
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

pub trait IteratorWithPos: Iterator {
    fn position(&self) -> (usize, usize) {
        (0, 0)
    }
}

impl<C: Iterator<Item = io::Result<char>>> IteratorWithPos for TokenGenerator<C> {
    fn position(&self) -> (usize, usize) {
        self.position()
    }
}