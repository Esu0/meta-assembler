//! コード生成ルールファイルの構文解析

use std::io;

use crate::lex::token_gen::{TokenGenerator, Token};

use super::{Error, ErrorKind};

pub enum Node {
    Integer(u64),
    Identifier(String),
    Comma,
    Semicolon,
}

impl From<Token> for Node {
    fn from(value: Token) -> Self {
        match value {
            Token::Integer(n) => Self::Integer(n),
            Token::Opr(ref s) => {
                match s.as_str() {
                    "," => Self::Comma,
                    ";" => Self::Semicolon,
                    _ => unimplemented!()
                }
            }
            Token::Word(w) => {
                Self::Identifier(w)
            }
        }
    }
}

pub struct Parser<C> {
    token_generator: TokenGenerator<C>,
}

impl<C: Iterator<Item = io::Result<char>>> Parser<C> {
    pub fn new(token_generator: TokenGenerator<C>) -> Self {
        Self { token_generator }
    }

    fn next_node(&mut self) -> Option<Result<Node, Error>> {
        self.token_generator.next().map(|x| x.map(Node::from).map_err(super::Error::from))
    }

    fn next_number(&mut self) -> Result<u64, Error> {
        if let Some(token) = self.token_generator.next() {
            match token? {
                Token::Integer(n) => Ok(n),
                token => 
                Err(Error::new(ErrorKind::UnexpectedToken { expected: "Integer".to_owned(), found: "".to_owned() }, self.token_generator.inner().position())),
            }
            
        } else {
            Err(io::Error::from(io::ErrorKind::UnexpectedEof).into())
        }
    }
}