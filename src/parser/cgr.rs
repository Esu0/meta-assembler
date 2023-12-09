//! コード生成ルールファイルの構文解析

use crate::{lex::token_gen::{Token, TokenGeneratorTrait}, parser::assembly::Table};

use super::{
    assembly::{GeneralRuleConfig, SyntaxRuleConfig, RulesConfig, TablesConfig, TableKey},
    Error, ErrorKind,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Integer(u64),
    Identifier(String),
    Comma,
    Semicolon,
    Colon,
    LF,
}

trait TokenGeneratorTraitExt: TokenGeneratorTrait {
    fn next_token(&mut self) -> Result<Option<Token>, Error> {
        self.next().transpose().map_err(From::from)
    }

    fn expect_number_or_word(&mut self) -> Result<Box<str>, Error> {
        match self.expect_number_as_string()? {
            Ok(s) => Ok(s),
            Err(Some(Token::Word(s))) => Ok(s.into()),
            Err(Some(t)) => Err(self.unexpected_token("IDENTIFIER or NUMBER".into(), t)),
            Err(None) => Err(self.unexpected_eof()),
        }
    }

    fn gen_error(&self, kind: ErrorKind) -> Error {
        let (line, column) = self.reader_position();
        Error {
            inner: super::ErrorInner::Simple(super::ErrorSimple { kind, line, column }),
        }
    }

    fn unexpected_token(&self, expected: Box<str>, found: Token) -> Error {
        self.gen_error(ErrorKind::UnexpectedToken { expected, found: String::from(found).into() })
    }

    fn unexpected_eof(&self) -> Error {
        Error::unexpected_eof(self.reader_position())
    }

    fn add_reader_position<T, EK: Into<ErrorKind>>(&self, result: Result<T, EK>) -> Result<T, Error> {
        result.map_err(|e| self.gen_error(e.into()))
    }
}

impl<T: TokenGeneratorTrait> TokenGeneratorTraitExt for T {}

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
    current: Option<Token>,
    rules: RulesConfig,
    tables: TablesConfig,
    syntax_rule: SyntaxRuleConfig,
    general_rules: GeneralRuleConfig,
    default_tables: Option<TableKey>,
}

impl<T: TokenGeneratorTrait> Parser<T> {
    pub fn new(token_generator: T) -> Self {
        Self {
            token_generator,
            current: None,
            rules: Default::default(),
            tables: Default::default(),
            syntax_rule: Default::default(),
            general_rules: Default::default(),
            default_tables: None,
        }
    }

    fn get_current_token(&mut self) -> Result<(), Error> {
        self.current = self.token_generator.next().transpose()?;
        Ok(())
    }

    fn take_token(&mut self) -> Result<Option<Token>, Error> {
        self.get_current_token()?;
        if let Some(t) = &mut self.current {
            Ok(Some(std::mem::replace(
                t,
                self.token_generator.next_token()?.unwrap(),
            )))
        } else {
            Ok(None)
        }
    }

    // consume系

    /// f(current)がtrueならOk(Some(current))を返す。falseならOk(None)を返す。
    /// Option<Token>::NoneはEOFを表す。
    fn consume(
        &mut self,
        f: impl FnOnce(&Option<Token>) -> bool,
    ) -> Result<Option<Option<Token>>, Error> {
        self.get_current_token()?;
        if f(&self.current) {
            Ok(Some(std::mem::replace(
                &mut self.current,
                self.token_generator.next_token()?,
            )))
        } else {
            Ok(None)
        }
    }

    fn consume_operator(&mut self, op: char) -> Result<bool, Error> {
        self.consume(|t| match t {
            Some(Token::Opr(s)) if s == &op.to_string() => true,
            _ => false,
        })
        .map(|x| x.is_some())
    }

    fn consume_word_of<S: AsRef<str> + ?Sized>(&mut self, s: &S) -> Result<bool, Error> {
        self.consume(|t| match t {
            Some(Token::Word(w)) if w.as_str() == s.as_ref() => true,
            _ => false,
        })
        .map(|x| x.is_some())
    }

    fn consume_word(&mut self) -> Result<Option<Box<str>>, Error> {
        self.consume(|t| matches!(t, Some(Token::Word(_)))).map(|x| match x {
            Some(Some(Token::Word(s))) => Some(s.into()),
            None => None,
            _ => unreachable!("consume(f) returned Ok(Some(_)) but f(current) is false."),
        })
    }

    // expect系

    /// トークンがToken::Wordでない時に限ってUnexpectedTokenエラーを返す。
    fn expect_word(&mut self) -> Result<Box<str>, Error> {
        match self.take_token()? {
            Some(Token::Word(s)) => Ok(s.into()),
            Some(t) => Err(self.unexpected_token("word".to_string(), t)),
            None => Err(self.unexpected_eof()),
        }
    }

    fn expect_word_with(&mut self, expected: String) -> Result<Box<str>, Error> {
        match self.take_token()? {
            Some(Token::Word(s)) => Ok(s.into()),
            Some(t) => Err(self.unexpected_token(expected, t)),
            None => Err(self.unexpected_eof()),
        }
    }

    fn expect_token(&mut self) -> Result<Token, Error> {
        match self.take_token()? {
            Some(t) => Ok(t),
            None => Err(self.unexpected_eof()),
        }
    }

    fn expect_number(&mut self) -> Result<u64, Error> {
        match self.take_token()? {
            Some(Token::Integer(n)) => Ok(n),
            Some(t) => Err(self.unexpected_token("number".to_string(), t)),
            None => Err(self.unexpected_eof()),
        }
    }

    fn expect_number_in_range(&mut self, min: u64, max: u64) -> Result<u64, Error> {
        let n = self.expect_number()?;
        if n < min || n > max {
            Err(self.num_out_of_range(min, max, n))
        } else {
            Ok(n)
        }
    }

    fn expect_operator(&mut self) -> Result<char, Error> {
        match self.take_token()? {
            Some(Token::Opr(s)) if s.len() == 1 => Ok(s.chars().next().unwrap()),
            Some(t) => {
                Err(self
                    .unexpected_token("OPERATOR such as \',\', \'.\', \';\', etc...".to_owned(), t))
            }
            None => Err(self.unexpected_eof()),
        }
    }

    // ignore系

    fn ignore(&mut self, f: impl FnOnce(&Option<Token>) -> bool) -> Result<(), Error> {
        self.get_current_token()?;
        if f(&self.current) {
            self.current = self.token_generator.next_token()?;
        }
        Ok(())
    }

    fn ignore_number(&mut self) -> Result<(), Error> {
        self.ignore(|t| matches!(t, Some(Token::Integer(_))))
    }

    fn unexpected_token(&self, expected: String, found: Token) -> Error {
        Error::unexpected_token(expected, found, self.token_generator.reader_position())
    }

    fn unexpected_eof(&self) -> Error {
        Error::unexpected_eof(self.token_generator.reader_position())
    }

    fn num_out_of_range(&self, min: u64, max: u64, found: u64) -> Error {
        let (line, column) = self.token_generator.reader_position();
        Error {
            inner: super::ErrorInner::Simple(super::ErrorSimple {
                kind: ErrorKind::NumOutOfRange { min, max, found },
                line,
                column,
            }),
        }
    }

    pub fn configulation(&mut self) -> Result<(), Error> {
        if self.consume_operator('*')? {
            let s = self.expect_word_with("word_size, address_size, etc...".to_owned())?;
            let s_str = s.as_ref();

            fn config<T: TokenGeneratorTrait>(
                this: &mut Parser<T>,
                min: u64,
                max: u64,
                f: impl FnOnce(
                    &mut GeneralRuleConfig,
                    u8,
                ) -> Result<(), super::assembly::DoubleDefinitionError>,
            ) -> Result<(), Error> {
                let val = this.expect_number_in_range(min, max)? as u8;
                let result = f(&mut this.general_rules, val);
                result.map_err(|e| this.token_generator.gen_error(e.into()))?;
                Ok(())
            }

            fn config2<T: TokenGeneratorTrait, U>(token_gen: &mut T, dst: &mut Option<U>, conf: U, found: Box<str>) -> Result<(), Error> {
                if dst.is_some() {
                    Err(token_gen.gen_error(ErrorKind::AlreadyExsistentProperty { found }))
                } else {
                    *dst = Some(conf);
                    Ok(())
                }
            }

            match s_str {
                "word_size" => config(self, 1, u8::MAX as u64, GeneralRuleConfig::set_word_size),
                "address_size" => {
                    config(self, 1, u8::MAX as u64, GeneralRuleConfig::set_address_size)
                }
                "empty_symbol_mode" => config(self, 0, 3, GeneralRuleConfig::set_empty_symbol_mode),
                "address_mode" => config(self, 0, 3, GeneralRuleConfig::set_address_mode),
                "how_operations" | "how_labels" | "how_equs" | "how_tables" | "how_entrys" => {
                    // output warning(予定)
                    self.ignore_number()
                }
                "org" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.org, w, s)
                }
                "end" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.end, w, s)
                }
                "equ" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.equ, w, s)
                }
                "db" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.db, w, s)
                }
                "ds" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.ds, w, s)
                }
                "dc" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.dc, w, s)
                }
                "code_dot" => {
                    let w = self.expect_operator()?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.code_dot, w, s)
                }
                "operand_dot" => {
                    let w = self.expect_operator()?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.operand_dot, w, s)
                }
                "dc_dot" => {
                    let w = self.expect_operator()?;
                    config2(&mut self.token_generator, &mut self.syntax_rule.dc_dot, w, s)
                }
                "table" => {
                    // table definition
                    let name = self.token_generator.expect_number_or_word()?;
                    let mut table = Table::new();
                    while let Some((name, val)) = self.stmt_define_table()? {
                        self.token_generator.add_reader_position(table.add(name, val))?;
                    }
                    self.token_generator.add_reader_position(self.tables.add_table(name, table))
                }
                _ => Err(self
                    .token_generator
                    .gen_error(ErrorKind::NonExsistentProperty { found: s })),
            }
        } else {
            Ok(())
        }
    }

    fn stmt_define_table(&mut self) -> Result<Option<(Box<str>, u64)>, Error> {
        if let Some(name) = self.consume_word()? {
            Ok(Some((name, self.expect_number()?)))
        } else {
            Ok(None)
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
