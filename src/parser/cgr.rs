//! コード生成ルールファイルの構文解析

use std::borrow::Borrow;

use crate::{lex::{token_gen::{Token, TokenGeneratorTrait}, char_gen::EncodeError, word_gen::Word}, parser::assembly::Table};

use super::{
    assembly::{GeneralRuleConfig, SyntaxRuleConfig, RulesConfig, TablesConfig, TableKey, table_key},
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
    fn expect_number_or_identifier(&mut self) -> Result<Box<str>, ErrorKind> {
        match self.expect_number_as_string()? {
            Ok(s) => Ok(s),
            Err(Some(Token::Word(s))) => Ok(s.into()),
            Err(Some(t)) => Err(ErrorKind::unexpected_token("IDENTIFIER or NUMBER".into(), t)),
            Err(None) => Err(ErrorKind::UnexpectedEof),
        }
    }

    fn expect_identifier(&mut self) -> Result<Box<str>, ErrorKind> {
        match self.expect_token()? {
            Token::Word(s) => Ok(s.into()),
            t => Err(ErrorKind::unexpected_token("IDENTIFIER".into(), t)),
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

    fn consume_operator(&mut self, op: char) -> Result<bool, EncodeError> {
        debug_assert!(self.is_separator(op));
        self.skip_spaces();
        match self.peek() {
            Some(Ok(c)) if c == op => {
                self.next_char();
                Ok(true)
            },
            Some(Err(_)) => Err(EncodeError),
            _ => Ok(false),
        }
    }

    fn consume_identifier(&mut self) -> Option<Box<str>> {
        self.skip_spaces();
        match self.peek() {
            Some(Ok(c)) if !(self.is_separator(c) || c.is_ascii_digit()) => {
                match self.next_word() {
                    Some(Ok(Word::Word(s))) => Some(s.into()),
                    _ => panic!("next_word() returned non-word token when peeked word."),
                }
            },
            _ => None,
        }
    }

    fn expect_token(&mut self) -> Result<Token, ErrorKind> {
        match self.next_token() {
            Some(t) => Ok(t?),
            None => Err(ErrorKind::UnexpectedEof),
        }
    }

    fn expect_number(&mut self) -> Result<u64, ErrorKind> {
        match self.expect_token()? {
            Token::Integer(n) => Ok(n),
            t => Err(ErrorKind::unexpected_token("number".to_owned(), t)),
        }
    }

    fn expect_number_in_range(&mut self, min: u64, max: u64) -> Result<u64, ErrorKind> {
        let n = self.expect_number()?;
        if n < min || n > max {
            Err(ErrorKind::num_out_of_range(min, max, n))
        } else {
            Ok(n)
        }
    }

    fn expect_word_of<S: Borrow<str> + ?Sized>(&mut self, w: &S) -> Result<(), ErrorKind> {
        match self.expect_token()? {
            Token::Word(s) if s.as_str() == w.borrow() => Ok(()),
            t => Err(ErrorKind::unexpected_token(w.borrow().to_owned(), t))
        }
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
            rules: Default::default(),
            tables: Default::default(),
            syntax_rule: Default::default(),
            general_rules: Default::default(),
            default_tables: None,
        }
    }

    // expect系

    /// トークンがToken::Wordでない時に限ってUnexpectedTokenエラーを返す。
    // fn expect_word(&mut self) -> Result<Box<str>, Error> {
    //     match self.take_token()? {
    //         Some(Token::Word(s)) => Ok(s.into()),
    //         Some(t) => Err(self.unexpected_token("word".to_string(), t)),
    //         None => Err(self.unexpected_eof()),
    //     }
    // }

    fn expect_word_with(&mut self, expected: String) -> Result<Box<str>, ErrorKind> {
        match self.expect_token()? {
            Token::Word(s) => Ok(s.into()),
            t => Err(ErrorKind::unexpected_token(expected, t)),
        }
    }

    fn expect_token(&mut self) -> Result<Token, ErrorKind> {
        match self.token_generator.next_token() {
            Some(t) => Ok(t?),
            None => Err(ErrorKind::UnexpectedEof),
        }
    }

    fn expect_number(&mut self) -> Result<u64, ErrorKind> {
        match self.expect_token()? {
            Token::Integer(n) => Ok(n),
            t => Err(ErrorKind::unexpected_token("number".to_owned(), t)),
        }
    }

    fn expect_number_in_range(&mut self, min: u64, max: u64) -> Result<u64, ErrorKind> {
        let n = self.expect_number()?;
        if n < min || n > max {
            Err(ErrorKind::num_out_of_range(min, max, n))
        } else {
            Ok(n)
        }
    }

    fn expect_operator_single(&mut self) -> Result<char, ErrorKind> {
        match self.expect_token()? {
            Token::Opr(s) if s.len() == 1 => Ok(s.chars().next().unwrap()),
            Token::Opr(s) => Err(ErrorKind::unexpected_token("single character OPERATOR".to_owned(), Token::Opr(s))),
            t => {
                Err(ErrorKind::unexpected_token("OPERATOR such as \',\', \'.\', \';\', etc...".to_owned(), t))
            }
        }
    }

    // ignore系

    // fn ignore(&mut self, f: impl FnOnce(&Option<Token>) -> bool) -> Result<(), Error> {
    //     self.get_current_token()?;
    //     if f(&self.current) {
    //         self.current = self.token_generator.next_token()?;
    //     }
    //     Ok(())
    // }

    fn ignore_number(&mut self) -> Result<(), ErrorKind> {
        match self.token_generator.peek() {
            Some(Ok(c)) if c.is_ascii_digit() => {
                let t = self.token_generator.next_token().unwrap_or_else(|| panic!("TokenGenerator returned EOF but remains some charactors."))?;
                debug_assert!(matches!(t, Token::Integer(_)), "TokenGenerator returned non-number token when peeked number.");
                Ok(())
            },
            _ => Ok(()),
        }
    }

    pub fn pgrm(&mut self) -> Result<(), ErrorKind> {
        loop {
            self.token_generator.skip_whitespaces();
            if self.token_generator.consume_operator('#')? {
                let i = self.token_generator.expect_identifier()?;
                match i.as_ref() {
                    "general_definition" | "table_definition" | "end" => {},
                    "rule_definition" => break,
                    _ => return Err(ErrorKind::UnexpectedToken { expected: "general_definition, table_definition, etc...".into(), found: i })
                }
            } else {
                self.configulation()?;
            }
        }
        while self.token_generator.peek().is_some() {
            self.rule_definitions()?;
        }
        Ok(())
    }

    pub fn configulation(&mut self) -> Result<(), ErrorKind> {
        self.token_generator.skip_whitespaces();
        if self.token_generator.consume_operator('*')? {
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
            ) -> Result<(), ErrorKind> {
                f(&mut this.general_rules, this.token_generator.expect_number_in_range(min, max)? as u8).map_err(From::from)
            }

            fn config2<U>(dst: &mut Option<U>, conf: U, found: Box<str>) -> Result<(), ErrorKind> {
                if dst.is_some() {
                    Err(ErrorKind::AlreadyExsistentProperty { found })
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
                    config2(&mut self.syntax_rule.org, w, s)
                }
                "end" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.syntax_rule.end, w, s)
                }
                "equ" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.syntax_rule.equ, w, s)
                }
                "db" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.syntax_rule.db, w, s)
                }
                "ds" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.syntax_rule.ds, w, s)
                }
                "dc" => {
                    let w = self.expect_word_with("IDENTIFIER".to_owned())?;
                    config2(&mut self.syntax_rule.dc, w, s)
                }
                "code_dot" => {
                    let w = self.expect_operator_single()?;
                    config2(&mut self.syntax_rule.code_dot, w, s)
                }
                "operand_dot" => {
                    let w = self.expect_operator_single()?;
                    config2(&mut self.syntax_rule.operand_dot, w, s)
                }
                "dc_dot" => {
                    let w = self.expect_operator_single()?;
                    config2(&mut self.syntax_rule.dc_dot, w, s)
                }
                "table" => {
                    // table definition
                    let name = self.token_generator.expect_number_or_identifier()?;
                    let mut table = Table::new();
                    while let Some((name, val)) = self.stmt_define_table()? {
                        table.add(name, val)?;
                    }
                    self.tables.add_table(name, table)?;
                    Ok(())
                }
                _ => Err(ErrorKind::NonExsistentProperty { found: s }),
            }
        } else {
            Ok(())
        }
    }

    fn stmt_define_table(&mut self) -> Result<Option<(Box<str>, u64)>, ErrorKind> {
        self.token_generator.skip_whitespaces();
        if let Some(name) = self.token_generator.consume_word() {
            self.token_generator.skip_whitespaces();
            Ok(Some((name, self.expect_number()?)))
        } else {
            Ok(None)
        }
    }

    fn rule_definitions(&mut self) -> Result<(), ErrorKind> {
        let tokens = &mut self.token_generator;
        tokens.skip_whitespaces();
        if tokens.consume_operator('*')? {
            tokens.expect_word_of("use_tables")?;
            while let Some(name) = tokens.consume_word() {
                let Some(i) = self.tables.get_index(&name) else {
                    return Err(ErrorKind::TableNotFound { found: name });
                };
                table_key::add_index(&mut self.default_tables, i);
                if !tokens.consume_operator(',')? {
                    break;
                }
            }
        } else if tokens.consume_operator('#')? {
            let i = tokens.expect_identifier()?;
            return match i.as_ref() {
                "end" => Ok(()),
                _ => Err(ErrorKind::UnexpectedToken { expected: "end".into(), found: i }),
            };
        } else if let Some(i) = tokens.consume_identifier() {
            // add rule
        }
        Ok(())
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
