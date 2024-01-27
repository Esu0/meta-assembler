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
        table_key, GeneralRuleConfig, OprCode, RuleConfig, RulesConfig, SyntaxRuleConfig, TableKey, TablesConfig
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

    fn expect_operator_single_of(&mut self, op: char) -> Result<(), ErrorKind> {
        if self.consume_operator(op) {
            Ok(())
        } else {
            Err(ErrorKind::UnexpectedToken { expected: op.to_string().into_boxed_str(), found: self.next_char().ok_or(ErrorKind::UnexpectedEof)??.to_string().into_boxed_str() })
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
        self.rule_definitions()?;
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

    fn rule_definitions(&mut self) -> Result<(), ErrorKind> {
        loop {
            if self.skip_whitespaces().consume_use_tables()? {
                continue;
            }
            if self.skip_whitespaces().consume_rule_definition()? {
                continue;
            }
            break Ok(());
        }
    }

    fn consume_use_tables(&mut self) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        if tokens.consume_operator('*') {
            tokens.expect_ident_of("use_tables")?;
            self.default_tables = None;
            while let Some(name) = tokens.skip_whitespaces().consume_identdigit() {
                let Some(i) = self.tables.get_index(&name) else {
                    return Err(ErrorKind::TableNotFound { found: name });
                };
                table_key::add_index(&mut self.default_tables, i);
                if !tokens.skip_whitespaces().consume_operator(',') {
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
            let mut offset = 8;
            let mut rule = RuleConfig::new();
            let default_bit = self.general_rules.empty_symbol_mode.map_or(0, |a| 0u8.wrapping_sub(a & 1));
            // TODO `RuleConfig::operands`の変更処理
            if self.token_generator.skip_whitespaces().consume('s') {
                rule.relative = true;
                if self.token_generator.skip_whitespaces().consume_operator(',') {
                    if self.token_generator.skip_whitespaces().consume_operator(';') {
                        self.rules.rules.insert(inst, rule.into());
                        return Ok(true);
                    }
                } else if self.token_generator.consume_operator(';') {
                    self.rules.rules.insert(inst, rule.into());
                    return Ok(true);
                }
            } else if self.token_generator.consume('d') {
                rule.relative = false;
                if self.token_generator.skip_whitespaces().consume_operator(',') {
                    if self.token_generator.skip_whitespaces().consume_operator(';') {
                        self.rules.rules.insert(inst, rule.into());
                        return Ok(true);
                    }
                } else if self.token_generator.consume_operator(';') {
                    self.rules.rules.insert(inst, rule.into());
                    return Ok(true);
                }
            } else if self.token_generator.skip_whitespaces().consume_operator(';') {
                self.rules.rules.insert(inst, rule.into());
                return Ok(true);
            }
            loop {
                self.token_generator.skip_whitespaces();
                if self.consume_binary_code(&mut rule.code, &mut offset, default_bit)? {
                    if self.token_generator.skip_whitespaces().consume_operator(',') {
                        if self.token_generator.skip_whitespaces().consume_operator(';') {
                            break;
                        }
                        continue;
                    } else if self.token_generator.consume_operator(';') {
                        break;
                    } else {
                        return Err(ErrorKind::UnexpectedToken {
                            expected: "\",\" or \";\"".into(),
                            found: self.token_generator.next_char().ok_or(ErrorKind::UnexpectedEof)??.to_string().into_boxed_str(),
                        });
                    }
                }
                if let Some(oprcode) = self.consume_special_opecode(&mut rule.code, &mut offset, default_bit)? {
                    rule.opr_code.extend(oprcode);
                    if self.token_generator.skip_whitespaces().consume_operator(',') {
                        if self.token_generator.skip_whitespaces().consume_operator(';') {
                            break;
                        }
                        continue;
                    } else if self.token_generator.consume_operator(';') {
                        break;
                    } else {
                        return Err(ErrorKind::UnexpectedToken {
                            expected: "\",\" or \";\"".into(),
                            found: self.token_generator.next_char().ok_or(ErrorKind::UnexpectedEof)??.to_string().into_boxed_str(),
                        });
                    }
                }
                let token = self.token_generator.expect_token("".into())?;
                return Err(ErrorKind::OpeCodeParseError {
                    found: token.into(),
                });
            }
            self.rules.rules.insert(inst, rule.into());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume_binary_code(&mut self, code: &mut Vec<u8>, offset: &mut u8, default_bit: u8) -> Result<bool, ErrorKind> {
        let tokens = &mut self.token_generator;
        let mut written = false;
        loop {
            match tokens.peek().transpose()? {
                Some('0') => {
                    tokens.next_char();
                    if *offset == 8 {
                        *offset = 0;
                        code.push(default_bit & 0b0111_1111);
                    } else if let Some(mr) = code.last_mut() {
                        *mr &= !(0x80 >> *offset);
                    }
                    written = true;
                }
                Some('1') => {
                    tokens.next_char();
                    if *offset == 8 {
                        *offset = 0;
                        code.push(default_bit | 0b1000_0000);
                    } else if let Some(mr) = code.last_mut() {
                        *mr |= 0x80 >> *offset;
                    }
                    written = true;
                }
                _ => break Ok(written),
            }
            *offset += 1;
        }
    }

    /// * `Ok(None)`: Any pattern doesn't match
    /// * `Ok(Some(None))`: Pattern matched but no oprcode ("fix")
    /// * `Ok(Some(Some(oprcode)))`: Pattern matched and oprcode ("opr" or "opc")
    fn consume_special_opecode(&mut self, code: &mut Vec<u8>, offset: &mut u8, default_bit: u8) -> Result<Option<Option<OprCode>>, ErrorKind> {
        if let Some(s) = self.token_generator.consume_identifier() {
            match &*s {
                "opr" => {
                    self.token_generator.skip_whitespaces().expect_operator_single_of('(')?;
                    let opr = self.token_generator.skip_whitespaces().expect_number_in_range(1, 256)? as u8;
                    self.token_generator.skip_whitespaces().expect_operator_single_of(':')?;
                    let length = self.token_generator.skip_whitespaces().expect_number_in_range(1, 64)? as u8;
                    let pos: u16 = (code.len() * 8 + *offset as usize - 8).try_into().map_err(|_| ErrorKind::TooLongOpeCode)?;
                    let additional_bit = (*offset + length) as i8 - 8;
                    *offset = additional_bit.rem_euclid(8) as u8;
                    if additional_bit > 0 {
                        code.extend((0..(additional_bit as u8).div_ceil(8)).map(|_| default_bit));
                    }
                    self.token_generator.skip_whitespaces().expect_operator_single_of(')')?;
                    Ok(Some(Some(OprCode::new(opr, length, pos))))
                }
                "opc" => {
                    unimplemented!("opc(<index>:<length>)")
                }
                "fix" => {
                    unimplemented!("fix(<value>:<length>)");
                    #[allow(unreachable_code)]
                    Ok(Some(None))
                }
                _ => {
                    Err(ErrorKind::UnexpectedToken {
                        expected: "\"opr\", \"opc\" or \"fix\"".into(),
                        found: s,
                    })
                }
            }
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{lex::char_gen::CharGenerator, parser::assembly::Table};

    use super::*;

    #[test]
    fn define_instruction_code_test() {
        let s: &[u8] = b"LD		  10100000,000,opr(1:3),00,opr(2:16);";
        let mut parser = Parser::new(CharGenerator::new(s).into_word_generator());
        assert!(parser.consume_rule_definition().unwrap());
        println!("{:#?}", parser.rules);
    }

    #[test]
    fn use_tables_test() {
        let s: &[u8] = b"*use_tables 1,3";
        let mut parser = Parser::new(CharGenerator::new(s).into_word_generator());
        parser.tables.add_table(
            "1".into(),
            Table::new(),
        ).unwrap();
        parser.tables.add_table(
            "2".into(),
            Table::new(),
        ).unwrap();
        parser.tables.add_table(
            "3".into(),
            Table::new(),
        ).unwrap();
        assert!(parser.consume_use_tables().unwrap());
        let mut key = TableKey::from_indice(&[0,2]);
        key.enable_immediate();
        assert_eq!(parser.default_tables, Some(key));
    }

    #[test]
    fn general_definition_test() {
        let s: &[u8] = b"
        *address_size 14
        *word_size 7
        *address_mode 2
        *empty_symbol_mode 0";
        let mut parser = Parser::new(CharGenerator::new(s).into_word_generator());
        assert!(parser.skip_whitespaces().configulation().unwrap());
        assert!(parser.skip_whitespaces().configulation().unwrap());
        assert!(parser.skip_whitespaces().configulation().unwrap());
        assert!(parser.skip_whitespaces().configulation().unwrap());
        assert_eq!(parser.general_rules, GeneralRuleConfig {
            address_size: Some(14),
            word_size: Some(7),
            immediate_size: None,
            address_mode: Some(2),
            empty_symbol_mode: Some(0),
        });
    }

    #[test]
    fn compile_test() {
        let s: &[u8] = b"
        *address_size 14
        *word_size 7
        *address_mode 2
        *empty_symbol_mode 0
        #rule_definition
        LD 0000000,0000000;";
        let mut parser = Parser::new(CharGenerator::new(s).into_word_generator());
        if let Err(e) = parser.compile() {
            println!("{e}");
        } else {
            println!("{:#?}", parser.general_rules);
            println!("{:#?}", parser.rules);
        }
    }
}
