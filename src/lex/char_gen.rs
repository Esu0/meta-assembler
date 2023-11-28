use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

use utf8_chars::BufReadCharsExt;

/// Readerから文字のイテレータを生成するための中間型
pub struct CharGenerator<R> {
    reader: R,
}

impl<R: BufRead> CharGenerator<R> {
    pub fn new(reader: R) -> Self {
        CharGenerator { reader }
    }

    /// 文字のイテレータを生成する
    pub fn chars(&mut self) -> impl Iterator<Item = io::Result<char>> + '_ {
        self.reader.chars()
    }

    /// ファイルなどの位置情報付きで文字のイテレータを生成する
    pub fn chars_with_pos(&mut self) -> CharsWithPos<impl Iterator<Item = io::Result<char>> + '_> {
        CharsWithPos {
            chars: self.reader.chars(),
            line: 1,
            column: 1,
        }
    }
}

impl CharGenerator<BufReader<File>> {
    /// pathで指定したファイルを読み込む
    pub fn load<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(CharGenerator::new(reader))
    }
}

/// 位置情報付きの文字のイテレータ
pub struct CharsWithPos<C> {
    chars: C,
    line: usize,
    column: usize,
}

impl<C> CharsWithPos<C> {
    /// 現在の位置を返す
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.column)
    }
}

impl<C: Iterator<Item = io::Result<char>>> Iterator for CharsWithPos<C> {
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
        let buf = "你好，世界！\nHello, world!\n".as_bytes();
        let mut gen = CharGenerator::new(buf);
        let mut chars = gen.chars();
        assert_eq!(chars.next().unwrap().unwrap(), '你');
        assert_eq!(chars.next().unwrap().unwrap(), '好');
        assert_eq!(chars.next().unwrap().unwrap(), '，');
    }

    #[test]
    #[should_panic]
    fn char_generator_error() {
        let buf = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
        let mut gen = CharGenerator::new(buf.as_ref());
        let mut chars = gen.chars();

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
