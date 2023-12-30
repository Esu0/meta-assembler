use std::io::{self, BufRead};

use utf8_chars::BufReadCharsExt;

use super::{token_gen::TokenGenerator, word_gen::WordGenerator};

/// 位置情報付きの文字のイテレータ
#[derive(Clone, Debug)]
pub struct CharGenerator<C> {
    chars: C,
    current: Option<Result<char, EncodeError>>,
    line: usize,
    column: usize,
}

impl<C> CharGenerator<C> {
    /// 現在の位置を返す
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    pub fn into_token_generator(self) -> TokenGenerator<C>
    where
        C: Iterator<Item = io::Result<char>>,
    {
        TokenGenerator::new(self)
    }
}

impl<C: Iterator<Item = io::Result<char>>> CharGenerator<C> {
    pub fn into_word_generator(self) -> WordGenerator<Self> {
        WordGenerator::new(self)
    }
}

impl<'a, R: BufRead> CharGenerator<utf8_chars::Chars<'a, R>> {
    pub fn new(reader: &'a mut R) -> Self {
        let mut chars = reader.chars();
        Self {
            current: chars.next().map(|r| r.map_err(|_| EncodeError)),
            chars: reader.chars(),
            line: 1,
            column: 1,
        }
    }
}

impl<C: Iterator<Item = io::Result<char>>> Iterator for CharGenerator<C> {
    type Item = Result<char, EncodeError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_char()
    }
}

impl<C: Iterator<Item = io::Result<char>>> CharGeneratorTrait for CharGenerator<C> {
    fn reader_position(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    fn next_char(&mut self) -> Option<Result<char, EncodeError>> {
        match std::mem::replace(
            &mut self.current,
            self.chars.next().map(|r| r.map_err(|_| EncodeError)),
        ) {
            Some(Ok('\n')) => {
                self.line += 1;
                self.column = 1;
                Some(Ok('\n'))
            }
            Some(Ok(c)) => {
                self.column += 1;
                Some(Ok(c))
            }
            c => c,
        }
    }

    fn peek(&mut self) -> Option<Result<char, EncodeError>> {
        self.current
    }
}

/// 文字生成器の機能を分離したトレイト
///
/// * 空白文字が発見された場合も無視せず、その文字を返す
///
/// * エンコード不可文字が発見されると`Err(EncodeError)`を返して読み進める
///
/// * Peekableな`Result<char, EncodeError>`のイテレータの機能を持つが、
/// `Iterator`トレイトは実装していない
///
/// * `Iterator`トレイトの機能が欲しい場合は`CharGeneratorTrait::chars`関数を使う
pub trait CharGeneratorTrait {
    /// 現在の行と列を返す
    fn reader_position(&self) -> (usize, usize);
    /// 次の文字を読み進める。
    fn next_char(&mut self) -> Option<Result<char, EncodeError>>;
    /// 次の文字を、読み進めずに返す。
    fn peek(&mut self) -> Option<Result<char, EncodeError>>;

    /// `Result<char, EncodeError>`のイテレータを返す。
    fn chars(&mut self) -> Chars<Self> {
        Chars { inner: self }
    }

    /// `predicate`が`false`を返すまでの文字を取り出す。
    /// エンコード不可文字が見つかったらイテレータは`None`を返す。
    fn take_while<P>(&mut self, predicate: P) -> TakeWhile<Self, P>
    where
        P: FnMut(char) -> bool,
    {
        TakeWhile {
            inner: self,
            predicate,
        }
    }

    /// `predicate`に`self`を使いたい場合に`take_while`の代わりに使う。
    /// `self`を使わない場合は`take_while`関数を使う。
    fn take_while_with_self<P>(&mut self, predicate: P) -> TakeWhileWithSelf<Self, P>
    where
        P: FnMut(&mut Self, char) -> bool,
    {
        TakeWhileWithSelf {
            inner: self,
            predicate,
        }
    }

    /// `predicate`が`true`を返している間、文字をスキップする。
    fn skip_while<P>(&mut self, mut predicate: P)
    where
        P: FnMut(Result<char, EncodeError>) -> bool,
    {
        while self.peek().map_or(false, &mut predicate) {
            self.next_char();
        }
    }

    /// 改行以外の空白文字をスキップする。
    /// ここでいう空白文字とは`char::is_ascii_whitespace`で`true`を返す文字
    fn skip_spaces(&mut self) {
        self.skip_while(|c| c.map_or(false, |c| c.is_ascii_whitespace() && c != '\n'))
    }

    /// 空白文字をスキップする。
    /// ここでいう空白文字とは`char::is_ascii_whitespace`で`true`を返す文字
    fn skip_whitespaces(&mut self) {
        self.skip_while(|c| c.map_or(false, |c| c.is_ascii_whitespace()))
    }

    /// 次の文字が`c`と一致したら文字を読み進め、`true`を返す。
    /// 一致しない場合は文字を読み進めず、`false`を返す。
    fn consume(&mut self, c: char) -> bool {
        if self.peek() == Some(Ok(c)) {
            self.next_char();
            true
        } else {
            false
        }
    }

    /// 次の文字を`f`に渡し、`true`が返されたら文字を読み進め、`true`を返す。
    /// `false`が返されたら文字を読み進めず、`false`を返す。
    fn consume_with<F: FnOnce(char) -> bool>(&mut self, f: F) -> bool {
        if let Some(Ok(c)) = self.peek() {
            if f(c) {
                self.next_char();
                return true;
            }
        }
        false
    }
}

/// エンコードできない文字およびそのエラー型を表す構造体
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[error("エンコードできない文字が含まれています。")]
pub struct EncodeError;

pub struct TakeWhile<'a, C: CharGeneratorTrait + ?Sized, P: FnMut(char) -> bool> {
    inner: &'a mut C,
    predicate: P,
}

impl<'a, C: CharGeneratorTrait + ?Sized, P: FnMut(char) -> bool> Iterator for TakeWhile<'a, C, P> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.peek() {
            Some(Ok(c)) if (self.predicate)(c) => {
                self.inner.next_char();
                Some(c)
            }
            _ => None,
        }
    }
}

pub struct TakeWhileWithSelf<'a, C: CharGeneratorTrait + ?Sized, P: FnMut(&mut C, char) -> bool> {
    inner: &'a mut C,
    predicate: P,
}

impl<'a, C: CharGeneratorTrait + ?Sized, P: FnMut(&mut C, char) -> bool> Iterator
    for TakeWhileWithSelf<'a, C, P>
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.peek() {
            Some(Ok(c)) if (self.predicate)(self.inner, c) => {
                self.inner.next_char();
                Some(c)
            }
            _ => None,
        }
    }
}

pub struct Chars<'a, T: CharGeneratorTrait + ?Sized> {
    inner: &'a mut T,
}

impl<'a, T: CharGeneratorTrait + ?Sized> Iterator for Chars<'a, T> {
    type Item = Result<char, EncodeError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next_char()
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn char_generator_test() {
        let mut buf = "你好，世界！\nHello, world!\n".as_bytes();
        let mut chars = CharGenerator::new(&mut buf);
        assert_eq!(chars.next_char().unwrap().unwrap(), '你');
        assert_eq!(chars.next_char().unwrap().unwrap(), '好');
        assert_eq!(chars.next_char().unwrap().unwrap(), '，');
        assert!(chars.consume('世'));
        assert!(chars.consume('界'));
        assert!(chars.consume('！'));
        chars.skip_whitespaces();
        assert!(chars.consume('H'));
    }
}
