use std::collections::HashSet;

use super::char_gen::{CharGeneratorTrait, EncodeError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Word {
    Word(Box<str>),
    Separator(char),
    NewLine,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Separators {
    inner: Option<HashSet<char>>,
}

impl Separators {
    fn add(&mut self, separator: char) {
        if let Some(inner) = &mut self.inner {
            inner.insert(separator);
        } else {
            self.inner = Some(
                Self::default_separators()
                    .iter()
                    .copied()
                    .chain(std::iter::once(separator))
                    .collect(),
            );
        }
    }

    fn extend<I: IntoIterator<Item = char>>(&mut self, itr: I) {
        if let Some(inner) = &mut self.inner {
            inner.extend(itr);
        } else {
            self.inner = Some(
                Self::default_separators()
                    .iter()
                    .copied()
                    .chain(itr)
                    .collect(),
            );
        }
    }

    fn default_separators() -> &'static [char] {
        &[
            ',', '$', '!', '?', '(', ')', '[', ']', '{', '}', '<', '>', '=', '+', '-', '*', '/',
            '%', '^', '&', '|', '~', '@', '#', ';', ':', '.', '"', '\'', '`', '\\',
        ]
    }

    pub fn is_separator(&self, c: char) -> bool {
        if let Some(inner) = &self.inner {
            inner.contains(&c)
        } else {
            Self::default_separators().contains(&c)
        }
    }

    pub fn remove(&mut self, separator: char) {
        if let Some(inner) = &mut self.inner {
            inner.remove(&separator);
        } else if Self::default_separators().contains(&separator) {
            self.inner = Some(
                Self::default_separators()
                    .iter()
                    .copied()
                    .filter(|c| *c != separator)
                    .collect(),
            );
        }
    }
}

pub struct WordGenerator<C: CharGeneratorTrait> {
    char_gen: C,
    separators: Separators,
}

impl<C: CharGeneratorTrait> WordGenerator<C> {
    pub fn new(char_gen: C) -> Self {
        Self {
            char_gen,
            separators: Default::default(),
        }
    }

    pub fn add_separator(&mut self, separator: char) {
        self.separators.add(separator);
    }

    pub fn add_separators<I: IntoIterator<Item = char>>(&mut self, itr: I) {
        self.separators.extend(itr);
    }

    pub fn default_separators() -> &'static [char] {
        &[
            ',', '$', '!', '?', '(', ')', '[', ']', '{', '}', '<', '>', '=', '+', '-', '*', '/',
            '%', '^', '&', '|', '~', '@', '#', ';', ':', '.', '"', '\'', '`', '\\',
        ]
    }

    pub fn remove_separator(&mut self, separator: char) {
        self.separators.remove(separator);
    }
}

impl<C: CharGeneratorTrait> WordGeneratorTrait for WordGenerator<C> {
    fn is_separator(&self, c: char) -> bool {
        self.separators.is_separator(c)
    }
}

impl<C: CharGeneratorTrait> CharGeneratorTrait for WordGenerator<C> {
    fn next_char(&mut self) -> Option<Result<char, EncodeError>> {
        self.char_gen.next_char()
    }

    fn peek(&mut self) -> Option<Result<char, EncodeError>> {
        self.char_gen.peek()
    }

    fn reader_position(&self) -> (usize, usize) {
        self.char_gen.reader_position()
    }
}

pub trait WordGeneratorTrait: CharGeneratorTrait {
    /// 区切り文字であるかどうかを判定する。
    fn is_separator(&self, c: char) -> bool;

    /// 改行以外の空白文字を無視し、`Word`単位に分割して読み進める。
    /// `Word`は、改行、区切り文字、単語の3種類がある。
    fn next_word(&mut self) -> Option<Result<Word, EncodeError>> {
        self.skip_spaces();
        self.next_char().map(|x| match x {
            Ok('\n') => Ok(Word::NewLine),
            Ok(c) if self.is_separator(c) => Ok(Word::Separator(c)),
            Ok(c) => Ok(Word::Word(
                std::iter::once(c)
                    .chain(self.take_while_with_self(|this, c| {
                        !(this.is_separator(c) || c.is_ascii_whitespace())
                    }))
                    .collect::<String>()
                    .into_boxed_str(),
            )),
            Err(_) => Err(EncodeError),
        })
    }

    fn gen_encode_error(&self) -> super::Error {
        let (line, column) = self.reader_position();
        super::Error::new_encode_error(line, column)
    }

    /// `Word`のイテレータを返す。
    fn words(&mut self) -> Words<Self> {
        Words { word_gen: self }
    }

    /// 次に来る`Word`が単語なら、その単語を返す。
    fn expect_word(&mut self) -> Result<Box<str>, Option<Result<Word, EncodeError>>> {
        match self.next_word() {
            Some(Ok(Word::Word(w))) => Ok(w),
            w => Err(w),
        }
    }
}

pub struct Words<'a, W: WordGeneratorTrait + ?Sized> {
    word_gen: &'a mut W,
}

impl<'a, W: WordGeneratorTrait + ?Sized> Iterator for Words<'a, W> {
    type Item = Result<Word, EncodeError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.word_gen.next_word()
    }
}

#[cfg(test)]
mod test {
    use crate::lex::char_gen::CharGenerator;

    use super::*;

    #[test]
    fn word_gen_test() {
        let mut buf = b"abc def".as_ref();
        let mut gen = CharGenerator::new(&mut buf).into_word_generator();
        assert_eq!(gen.next_word(), Some(Ok(Word::Word("abc".into()))));
        assert_eq!(gen.next_word(), Some(Ok(Word::Word("def".into()))));

        let mut buf = b"abc def\xFF ieo,;:@ovd123+233@0_0)".as_ref();
        let mut gen = CharGenerator::new(&mut buf).into_word_generator();
        assert_eq!(gen.expect_word(), Ok("abc".into()));
        assert_eq!(gen.expect_word(), Ok("def".into()));
        assert_eq!(gen.expect_word(), Err(Some(Err(EncodeError))));
        assert_eq!(gen.expect_word(), Ok("ieo".into()));
        assert_eq!(gen.expect_word(), Err(Some(Ok(Word::Separator(',')))));
        assert!(gen.consume(';'));
        assert!(gen.consume(':'));
        assert!(!gen.consume('d'));
        assert!(gen.consume('@'));
        assert_eq!(gen.expect_word(), Ok("ovd123".into()));
        gen.skip_while(|c| c != Ok('@'));
        assert!(gen.consume('@'));

        // println!("WordGeneratorTrait::expect_wordが返す値のサイズ: {} byte", std::mem::size_of_val(&gen.expect_word()));
    }
}
