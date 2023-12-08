//! コード生成ルールファイルの構文解析

use std::io;

use crate::lex::token_gen::{Token, TokenGeneratorTrait};

use super::{assembly::Rules, Error, ErrorKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Integer(u64),
    Identifier(String),
    Comma,
    Semicolon,
    Colon,
    LF,
}

impl From<Token> for Node {
    fn from(value: Token) -> Self {
        match value {
            Token::Integer(n) => Self::Integer(n),
            Token::Opr(ref s) => match s.as_str() {
                "," => Self::Comma,
                ";" => Self::Semicolon,
                ":" => Self::Colon,
                _ => unimplemented!(),
            },
            Token::Word(w) => Self::Identifier(w),
            Token::NewLine => Self::LF,
        }
    }
}

pub struct Parser<T: TokenGeneratorTrait> {
    token_generator: T,
    rules: Rules,
}

impl<T: TokenGeneratorTrait> Parser<T> {
    pub fn new(token_generator: T) -> Self {
        Self {
            token_generator,
            rules: Default::default(),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        if let Some(t) = self.token_generator.next() {
            Ok(t?)
        } else {
            Err(Error::unexpected_eof(
                self.token_generator.reader_position(),
            ))
        }
    }

    pub fn consume(
        &mut self,
        buf: &mut Token,
        f: impl FnOnce(&Token) -> bool,
    ) -> Result<Option<Token>, Error> {
        if f(&*buf) {
            Ok(Some(std::mem::replace(buf, self.next_token()?)))
        } else {
            Ok(None)
        }
    }

    pub fn consume_operator(&mut self, buf: &mut Token, op: char) -> Result<bool, Error> {
        self.consume(buf, |t| match t {
            Token::Opr(s) if s == &op.to_string() => true,
            _ => false,
        })
        .map(|x| x.is_some())
    }

    pub fn general_config(&mut self, buf: &mut Token) -> Result<(), Error> {
        if self.consume_operator(buf, '*')? {

        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parser_test() {
        let _a = vec![
            Token::Word("a".to_owned()),
            Token::Opr(",".to_owned()),
            Token::Integer(1),
            Token::Opr(",".to_owned()),
            Token::Opr(":".to_owned()),
            Token::Integer(2),
            Token::Opr(";".to_owned()),
            Token::Word("b".to_owned()),
            Token::Opr("[".to_owned()),
            Token::Integer(3),
            Token::Opr("]".to_owned()),
            Token::Opr(":".to_owned()),
            Token::Integer(4),
            Token::Opr(";".to_owned()),
        ];
    }
}
