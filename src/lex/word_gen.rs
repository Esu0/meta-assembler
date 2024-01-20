use std::collections::HashSet;

use super::char_gen::{CharGeneratorTrait, EncodeError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Word {
    IdentDigit(Box<str>),
    Separator(char),
    NewLine,
    Spaces,
}

impl Word {
    pub fn kind(&self) -> WordKind {
        match self {
            Self::IdentDigit(_) => WordKind::IdentDigit,
            Self::Separator(_) => WordKind::Separator,
            Self::NewLine => WordKind::NewLine,
            Self::Spaces => WordKind::Spaces,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WordKind {
    IdentDigit,
    Separator,
    NewLine,
    Spaces,
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
            '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';',
            '<', '=', '>', '?', '@', '[', '\\', ']', '^', '`', '{', '|', '}', '~',
        ]
    }

    fn is_default_separator(c: char) -> bool {
        matches!(c, '!'..='/' | ':'..='@' | '['..='^' | '`' | '{'..='~')
    }

    pub fn is_separator(&self, c: char) -> bool {
        if let Some(inner) = &self.inner {
            inner.contains(&c)
        } else {
            Self::is_default_separator(c)
        }
    }

    pub fn remove(&mut self, separator: char) {
        if let Some(inner) = &mut self.inner {
            inner.remove(&separator);
        } else if Self::is_default_separator(separator) {
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
        Separators::default_separators()
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

    fn peek(&self) -> Option<Result<char, EncodeError>> {
        self.char_gen.peek()
    }

    fn reader_position(&self) -> (usize, usize) {
        self.char_gen.reader_position()
    }
}

pub(super) fn error_next_word_kind() -> ! {
    report_broken_impl(&["next_word_kind"])
}

pub(super) fn report_broken_impl(methods: &[&'static str]) -> ! {
    panic!(
        "WordGeneratorTrait implementation broken:
        consider checking `{}` method",
        methods.join("`, `")
    );
}
/// 単語生成器の機能を分離したトレイト
///
/// 基本的に空白文字は無視されるが、必要以上に空白文字をスキップしない。
pub trait WordGeneratorTrait: CharGeneratorTrait {
    /// 区切り文字であるかどうかを判定する。
    fn is_separator(&self, c: char) -> bool;

    #[inline]
    fn next_word_kind(&self) -> Option<Result<WordKind, EncodeError>> {
        self.peek().map(|c| {
            c.map(|c| match c {
                '\n' => WordKind::NewLine,
                c if c.is_ascii_whitespace() => WordKind::Spaces,
                c if self.is_separator(c) => WordKind::Separator,
                _ => WordKind::IdentDigit,
            })
        })
    }

    /// `Word`は、改行、空白、区切り文字、単語の3種類がある。
    ///
    /// 連続する空白文字は一つの`Word::Spaces`として扱われる。
    #[inline]
    fn next_word(&mut self) -> Option<Result<Word, EncodeError>> {
        match self.next_word_kind()? {
            Ok(WordKind::NewLine) => {
                self.next_char();
                Some(Ok(Word::NewLine))
            }
            Ok(WordKind::Spaces) => {
                self.skip_spaces();
                Some(Ok(Word::Spaces))
            }
            Ok(WordKind::Separator) => self.next_char().map(|c| c.map(Word::Separator)),
            Ok(WordKind::IdentDigit) => Some(Ok(Word::IdentDigit(self.get_identdigit_or_panic()))),
            Err(_) => {
                self.next_char();
                Some(Err(EncodeError))
            }
        }
    }

    /// `Result<Word, EncodeError>`のイテレータを返す。
    ///
    /// このイテレータが`None`を返すことはEOFを意味する。
    fn words(&mut self) -> Words<Self> {
        Words { word_gen: self }
    }

    /// 次に単語が来る場合は、その単語を返し、そうでない場合は読み進めずに`None`を返す。
    ///
    /// 直後に空白文字が来た場合も`None`を返す。
    fn consume_identdigit(&mut self) -> Option<Box<str>> {
        if let Some(Ok(WordKind::IdentDigit)) = self.next_word_kind() {
            Some(self.get_identdigit_or_panic())
        } else {
            None
        }
    }

    /// 次に返される`Result<Word, EncodeError>`を無視する
    fn ignore_next_word(&mut self) {
        if let Some(kind) = self.next_word_kind() {
            match kind {
                Ok(WordKind::NewLine | WordKind::Separator) | Err(_) => {
                    self.next_char();
                }
                Ok(WordKind::Spaces) => self.skip_spaces(),
                Ok(WordKind::IdentDigit) => self.ignore_identdigit(),
            }
        }
    }

    /// 直後に来る指定した種類の`Word`を無視する。
    ///
    /// 実際に読み進めた場合は`true`を返し、読み進めなかった場合は`false`を返す。
    fn ignore_word_of(&mut self, kind: WordKind) -> bool {
        if matches!(self.next_word_kind(), Some(Ok(k)) if k == kind) {
            self.next_word();
            true
        } else {
            false
        }
    }
}

trait WordGenPrivate: WordGeneratorTrait {
    fn get_identdigit(&mut self) -> Box<str> {
        self.take_while_with_self(|this, c| !(this.is_separator(c) || c.is_ascii_whitespace()))
            .collect::<String>()
            .into_boxed_str()
    }

    fn get_identdigit_or_panic(&mut self) -> Box<str> {
        let tmp = self.get_identdigit();
        if tmp.is_empty() {
            error_next_word_kind();
        }
        tmp
    }

    fn ignore_identdigit(&mut self) {
        self.skip_while_with_self(|this, c| {
            c.map_or(false, |c| {
                !(c.is_ascii_whitespace() || this.is_separator(c) || c == '\n')
            })
        });
    }
}

impl<T: WordGeneratorTrait + ?Sized> WordGenPrivate for T {}

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
        assert_eq!(gen.next_word(), Some(Ok(Word::IdentDigit("abc".into()))));
        assert_eq!(gen.next_word(), Some(Ok(Word::Spaces)));
        assert_eq!(gen.next_word(), Some(Ok(Word::IdentDigit("def".into()))));

        let buf = b"abc def\xFF ieo,;:@ovd123+233@0_0)".as_ref();
        let mut tmp = buf;
        let mut gen = CharGenerator::new(&mut tmp).into_word_generator();
        assert_eq!(gen.consume_identdigit(), Some("abc".into()));
        assert_eq!(gen.consume_identdigit(), None);
        gen.skip_spaces();
        assert_eq!(gen.consume_identdigit(), Some("def".into()));
        assert_eq!(gen.consume_identdigit(), None);
        assert_eq!(gen.next_word(), Some(Err(EncodeError)));
        assert_eq!(gen.consume_identdigit(), None);
        assert_eq!(gen.next_word(), Some(Ok(Word::Spaces)));
        assert_eq!(gen.consume_identdigit(), Some("ieo".into()));
        assert_eq!(gen.next_word(), Some(Ok(Word::Separator(','))));
        assert!(gen.consume(';'));
        assert!(gen.consume(':'));
        assert!(!gen.consume('d'));
        assert!(gen.consume('@'));
        assert_eq!(gen.consume_identdigit(), Some("ovd123".into()));
        gen.skip_while(|c| c != Ok('@'));
        assert!(gen.consume('@'));

        tmp = buf;
        let mut gen = CharGenerator::new(&mut tmp).into_word_generator();
        assert_eq!(gen.consume_identdigit(), Some("abc".into()));
        gen.ignore_next_word();
        assert_eq!(gen.consume_identdigit(), Some("def".into()));
        assert_eq!(gen.consume_identdigit(), None);
        assert_eq!(gen.next_word(), Some(Err(EncodeError)));
        // println!("WordGeneratorTrait::expect_wordが返す値のサイズ: {} byte", std::mem::size_of_val(&gen.expect_word()));
    }
}
