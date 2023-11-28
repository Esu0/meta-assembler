use std::io::{self, BufRead};

use utf8_chars::BufReadCharsExt;

use super::token_gen::TokenGenerator;


/// 位置情報付きの文字のイテレータ
#[derive(Clone, Debug)]
pub struct CharGenerator<C> {
    chars: C,
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
        C: Iterator<Item = io::Result<char>>
    {
        TokenGenerator::new(self)
    }
}

impl<'a, R: BufRead> From<&'a mut R> for CharGenerator<utf8_chars::Chars<'a, R>> {
    fn from(value: &'a mut R) -> Self {
        Self {
            chars: value.chars(),
            line: 1,
            column: 1,
        }
    }
}

impl<'a, R: BufRead> CharGenerator<utf8_chars::Chars<'a, R>> {
    pub fn new(reader: &'a mut R) -> Self {
        Self {
            chars: reader.chars(),
            line: 1,
            column: 1,
        }
    }
}

impl<C: Iterator<Item = io::Result<char>>> Iterator for CharGenerator<C> {
    type Item = C::Item;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next();
        match c {
            Some(Ok('\n')) => {
                self.line += 1;
                self.column = 1;
            }
            Some(Ok(_)) => {
                self.column += 1;
            }
            _ => {}
        }
        c
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    #[should_panic]
    fn char_test() {
        let a = 0xfff10a52u32;
        println!("{:?}", char::from_u32(a).unwrap());
    }

    #[test]
    fn char_generator_test() {
        let mut buf = "你好，世界！\nHello, world!\n".as_bytes();
        let mut chars = CharGenerator::new(&mut buf);
        assert_eq!(chars.next().unwrap().unwrap(), '你');
        assert_eq!(chars.next().unwrap().unwrap(), '好');
        assert_eq!(chars.next().unwrap().unwrap(), '，');
    }

    #[test]
    #[should_panic]
    fn char_generator_error() {
        let mut buf = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff].as_ref();
        let mut chars = CharGenerator::new(&mut buf);

        // イメージ的にはエラー処理はこんな感じになると想定
        let _c = match chars.next().unwrap() {
            Ok(c) => c,
            Err(e) => {
                // invalid dataとしか表示されないので、自作エラー型を作りたい
                panic!("{}", e);
            }
        };
    }
}
