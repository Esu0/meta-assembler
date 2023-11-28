//! 構文解析

pub mod assembly;
pub mod cgr;

use std::{fmt::Display, io};

use thiserror::Error;

use crate::lex::token_gen::{Token, TokenGenerator};

#[derive(Error, Debug)]
pub enum ErrorKind {
    #[error("予期せぬトークン{found}が検出されました。トークン{expected}が予測されます。")]
    UnexpectedToken { expected: String, found: String },
    #[error("ファイルの終端に到達しました。")]
    UnexpectedEof,
}

#[derive(Error, Debug)]
#[error("{info}: {kind}")]
struct ErrorSimple {
    kind: ErrorKind,
    info: String,
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

    pub fn new(kind: ErrorKind, info: String) -> Self {
        Self {
            inner: ErrorInner::Simple(ErrorSimple { kind, info }),
        }
    }

    pub fn unexpected_token(expected: String, found: Token, info: String) -> Self {
        Self::new(
            ErrorKind::UnexpectedToken {
                expected,
                found: token_to_string(found),
            },
            info,
        )
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

pub trait IteratorWith: Iterator {
    type Info: Display;
    fn info(&self) -> Self::Info;
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReaderPos(usize, usize);

impl Display for ReaderPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}({})", self.0, self.1)
    }
}

pub trait IteratorExt: Iterator {
    fn with_info<Info>(self, info: Info) -> WithInfo<Self, Info>
    where
        Self: Sized,
    {
        WithInfo { iter: self, info }
    }
}

pub struct WithInfo<I: Iterator, Info> {
    iter: I,
    info: Info,
}

impl<T: Iterator> IteratorExt for T {}

impl<I: Iterator, Info> WithInfo<I, Info> {
    pub fn info(&self) -> &Info {
        &self.info
    }
}

impl<I: Iterator, Info> Iterator for WithInfo<I, Info> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<C: Iterator<Item = io::Result<char>>> IteratorWith for TokenGenerator<C> {
    type Info = ReaderPos;
    fn info(&self) -> Self::Info {
        let (l, c) = self.inner().position();
        ReaderPos(l, c)
    }
}

impl<I: Iterator, Info: Display + Clone> IteratorWith for WithInfo<I, Info> {
    type Info = Info;
    fn info(&self) -> Self::Info {
        self.info.clone()
    }
}
