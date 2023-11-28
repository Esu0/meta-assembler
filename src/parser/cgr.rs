//! コード生成ルールファイルの構文解析

use std::io;

use crate::lex::token_gen::Token;

use super::{Error, ErrorKind, IteratorWith};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Integer(u64),
    Identifier(String),
    Comma,
    Semicolon,
    Colon,
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
        }
    }
}

pub struct Parser<T: IteratorWith<Item = Result<Token, crate::lex::Error>>> {
    token_generator: T,
}

impl<T: IteratorWith<Item = Result<Token, crate::lex::Error>>> Parser<T> {
    pub fn new(token_generator: T) -> Self {
        Self { token_generator }
    }

    fn next_node(&mut self) -> Option<Result<Node, Error>> {
        self.token_generator
            .next()
            .map(|x| x.map(Node::from).map_err(super::Error::from))
    }

    fn next_number(&mut self) -> Result<u64, Error> {
        if let Some(token) = self.token_generator.next() {
            match token? {
                Token::Integer(n) => Ok(n),
                token => Err(Error::unexpected_token(
                    "Integer".to_owned(),
                    token,
                    self.token_generator.info().to_string(),
                )),
            }
        } else {
            Err(io::Error::from(io::ErrorKind::UnexpectedEof).into())
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::IteratorExt;
    use super::*;

    #[test]
    fn parser_test() {
        let a = vec![
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
        let i = a.into_iter().map(|t| Ok(t)).with_info("test");
        let mut parser = Parser::new(i);
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Identifier("a".to_owned()));
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Comma);
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Integer(1));
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Comma);
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Colon);
        assert_eq!(parser.next_number().unwrap(), 2);
        assert_eq!(parser.next_node().unwrap().unwrap(), Node::Semicolon);
        assert_eq!(parser.next_number().unwrap_err().to_string(), "test: 予期せぬトークンbが検出されました。トークンIntegerが予測されます。");
    }
}
