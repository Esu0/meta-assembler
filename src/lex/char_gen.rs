use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

use utf8_chars::BufReadCharsExt;

pub struct CharGenerator<R> {
    reader: R,
}

impl<R: BufRead> CharGenerator<R> {
    pub fn new(reader: R) -> Self {
        CharGenerator {
            reader,
        }
    }

    // 外部結合が伝播しないようにimpl Iteratorを返す
    pub fn chars(&mut self) -> impl Iterator<Item = io::Result<char>> + '_ {
        self.reader.chars()
    }
}

impl CharGenerator<BufReader<File>> {
    pub fn load<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(CharGenerator::new(reader))
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
}