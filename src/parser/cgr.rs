//! コード生成ルールファイルの構文解析

use std::{borrow::Borrow, collections::HashMap};

use crate::{
    lex::{
        char_gen::CharGeneratorTrait,
        token_gen::{Token, TokenGeneratorTrait, TokenKind},
        word_gen::Word,
    },
    parser::assembly::TableBits,
};

use super::{
    assembly::{
        table_key, GeneralRuleConfig, RulesConfig, SyntaxRuleConfig, TableKey, TablesConfig,
    },
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

trait CharGeneratorTraitExt: CharGeneratorTrait {
    fn is_next_error(&mut self) -> bool {
        matches!(self.peek(), Some(Err(_)))
    }
}

impl<T: CharGeneratorTrait> CharGeneratorTraitExt for T {}

pub(super) trait TokenGeneratorTraitExt: TokenGeneratorTrait {
    fn expect_number_or_identifier(&mut self) -> Result<Box<str>, ErrorKind> {
        match self.next_word().ok_or(ErrorKind::UnexpectedEof)?? {
            Word::IdentDigit(s) => Ok(s),
            Word::Separator(c) => Err(ErrorKind::unexpected_token(
                "IDENTIFIER or NUMBER".into(),
                Token::single_operator(c),
            )),
            Word::NewLine | Word::Spaces => Err(ErrorKind::TokenNotFound {
                expected: "IDENTIFIER or NUMBER".into(),
            }),
        }
    }

    fn expect_identifier(&mut self) -> Result<Box<str>, ErrorKind> {
        match self.expect_token("IDENTIFIER".into())? {
            Token::Ident(s) => Ok(s),
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
        self.gen_error(ErrorKind::UnexpectedToken {
            expected,
            found: String::from(found).into(),
        })
    }

    fn unexpected_eof(&self) -> Error {
        Error::unexpected_eof(self.reader_position())
    }

    fn add_reader_position<T, EK: Into<ErrorKind>>(
        &self,
        result: Result<T, EK>,
    ) -> Result<T, Error> {
        result.map_err(|e| self.gen_error(e.into()))
    }

    fn expect_token_or_space(&mut self) -> Result<Option<Token>, ErrorKind> {
        Ok(self
            .next_token_or_space()
            .ok_or(ErrorKind::UnexpectedEof)??)
    }

    fn expect_token(&mut self, msg_expected: Box<str>) -> Result<Token, ErrorKind> {
        self.expect_token_or_space()?
            .ok_or(ErrorKind::TokenNotFound {
                expected: msg_expected,
            })
    }

    fn expect_number(&mut self) -> Result<i64, ErrorKind> {
        match self.expect_token("NUMBER".into())? {
            Token::Integer(n) => Ok(n),
            t => Err(ErrorKind::unexpected_token("number".into(), t)),
        }
    }

    fn expect_number_in_range(&mut self, min: i64, max: i64) -> Result<i64, ErrorKind> {
        let n = self.expect_number()?;
        if n < min as _ || n > max as _ {
            Err(ErrorKind::num_out_of_range(min, max, n))
        } else {
            Ok(n)
        }
    }

    fn expect_ident_of<S: Borrow<str> + ?Sized>(&mut self, w: &S) -> Result<(), ErrorKind> {
        match self.expect_token(w.borrow().into())? {
            Token::Ident(s) if &*s == w.borrow() => Ok(()),
            t => Err(ErrorKind::unexpected_token(w.borrow().into(), t)),
        }
    }

    fn expect_ident_with(&mut self, msg_expected: Box<str>) -> Result<Box<str>, ErrorKind> {
        match self.expect_token_or_space()? {
            Some(Token::Ident(s)) => Ok(s),
            Some(t) => Err(ErrorKind::unexpected_token(msg_expected, t)),
            None => Err(ErrorKind::TokenNotFound {
                expected: msg_expected,
            }),
        }
    }

    fn expect_operator_single(&mut self) -> Result<char, ErrorKind> {
        match self.expect_token("OPERATOR such as \',\', \'.\', \';\', etc...".into())? {
            Token::Opr(s) if s[1].is_none() => Ok(s[0].unwrap()),
            Token::Opr(s) => Err(ErrorKind::unexpected_token(
                "single character OPERATOR".into(),
                Token::Opr(s),
            )),
            t => Err(ErrorKind::unexpected_token(
                "OPERATOR such as \',\', \'.\', \';\', etc...".into(),
                t,
            )),
        }
    }
}

impl<T: TokenGeneratorTrait> TokenGeneratorTraitExt for T {}

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

    // ignore系

    // fn ignore(&mut self, f: impl FnOnce(&Option<Token>) -> bool) -> Result<(), Error> {
    //     self.get_current_token()?;
    //     if f(&self.current) {
    //         self.current = self.token_generator.next_token()?;
    //     }
    //     Ok(())
    // }

    pub fn compile(&mut self) -> Result<(), Error> {
        self.pgrm().map_err(|e| self.token_generator.gen_error(e))
    }

    pub fn skip_whitespaces(&mut self) -> &mut Self {
        self.token_generator.skip_whitespaces();
        self
    }

    fn pgrm(&mut self) -> Result<(), ErrorKind> {
        loop {
            if self
                .token_generator
                .skip_whitespaces()
                .consume_operator('#')
            {
                let i = self.token_generator.expect_identifier()?;
                match &*i {
                    "general_definition" | "table_definition" | "end" => {}
                    "rule_definition" => break,
                    _ => {
                        return Err(ErrorKind::UnexpectedToken {
                            expected: "general_definition, table_definition, etc...".into(),
                            found: i,
                        })
                    }
                }
            } else if !self.configulation()? {
                break;
            }
        }
        while self.skip_whitespaces().rule_definitions()? {}
        Ok(())
    }

    pub fn configulation(&mut self) -> Result<bool, ErrorKind> {
        if self.token_generator.consume_operator('*') {
            let s = self
                .token_generator
                .expect_ident_with("word_size, address_size, etc...".into())?;
            let s_str = s.as_ref();

            self.skip_whitespaces();
            if self
                .general_rules
                .set_by_token_generator(s_str, &mut self.token_generator)?
                || self.ignore_deprecated_property(s_str)?
                || self
                    .syntax_rule
                    .set_by_token_generator(s_str, &mut self.token_generator)?
            {
                Ok(true)
            } else if s_str == "table" {
                // table definition
                let name = self.token_generator.expect_number_or_identifier()?;
                let mut tmp = Vec::new();
                let mut max_bits = 1;
                while let Some((name, val)) = self.skip_whitespaces().stmt_define_table()? {
                    tmp.push((name, val));
                    max_bits = max_bits.max(val.checked_ilog2().unwrap_or_default() as u8);
                }
                self.tables.add_table(
                    name,
                    tmp.into_iter()
                        .map(|(name, val)| (name, TableBits::from_num(val as _, max_bits)))
                        .collect::<HashMap<_, _>>()
                        .into(),
                )?;
                Ok(true)
            } else {
                Err(ErrorKind::NonExsistentProperty { found: s })
            }
        } else {
            Ok(false)
        }
    }

    fn ignore_deprecated_property(&mut self, key: &str) -> Result<bool, ErrorKind> {
        match key {
            "how_operations" | "how_labels" | "how_equs" | "how_tables" | "how_entrys" => {
                // output warning(予定)
                self.token_generator.ignore_token(TokenKind::Integer)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn stmt_define_table(&mut self) -> Result<Option<(Box<str>, i64)>, ErrorKind> {
        if let Some(name) = self.token_generator.consume_identdigit() {
            Ok(Some((
                name,
                self.token_generator.skip_whitespaces().expect_number()?,
            )))
        } else {
            Ok(None)
        }
    }

    fn rule_definitions(&mut self) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        tokens.skip_whitespaces();
        if tokens.consume_operator('*') {
            tokens.expect_ident_of("use_tables")?;
            while let Some(name) = tokens.consume_identdigit() {
                let Some(i) = self.tables.get_index(&name) else {
                    return Err(ErrorKind::TableNotFound { found: name });
                };
                table_key::add_index(&mut self.default_tables, i);
                if !tokens.consume_operator(',') {
                    break;
                }
            }
        } else if tokens.consume_operator('#') {
            let i = tokens.expect_identifier()?;
            return match i.as_ref() {
                "end" => Ok(true),
                _ => Err(ErrorKind::UnexpectedToken {
                    expected: "end".into(),
                    found: i,
                }),
            };
        } else if let Some(i) = tokens.consume_identifier() {
            // add rule
        }
        todo!()
    }

    fn consume_use_tables(&mut self) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        if tokens.consume_operator('*') {
            tokens.expect_ident_of("use_tables")?;
            self.default_tables = None;
            while let Some(name) = tokens.consume_identdigit() {
                let Some(i) = self.tables.get_index(&name) else {
                    return Err(ErrorKind::TableNotFound { found: name });
                };
                table_key::add_index(&mut self.default_tables, i);
                if !tokens.consume_operator(',') {
                    break;
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume_rule_definition(&mut self) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        if let Some(inst) = tokens.consume_identifier() {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// 空白を読み進めない
    fn consume_binary_code(&mut self, code: &mut [u8], pos: &mut u16) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        match tokens.peek() {
            Some(Ok('0')) | Some(Ok('1')) => {
                let s = tokens.consume_identdigit().unwrap_or_else(|| {
                    panic!("WordGeneratorTrait::expect_word() implementation broken.")
                });
                let length = s.len();
                let Ok(bits) = u64::from_str_radix(s.as_ref(), 2) else {
                    return Err(ErrorKind::OpeCodeParseError { found: s });
                };
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
