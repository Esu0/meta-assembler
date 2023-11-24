use std::{
    fs::File,
    io::{self, BufRead, BufReader, Bytes, Read},
    path::Path,
};

pub struct CharGenerator<B> {
    bytes: B,
    buf: u32,
}

impl<B> CharGenerator<B> {
    pub fn new<R: Read>(reader: R) -> CharGenerator<impl Iterator<Item = io::Result<u8>>> {
        CharGenerator {
            bytes: reader.bytes(),
            buf: 0,
        }
    }

    pub fn next_char(&mut self) -> Option<char>
    where
        B: Iterator<Item = io::Result<u8>>
    {
        // 1 byte char
        let mut buf = self.bytes.next()?.expect("io error") as u32;
        if let Some(c) = char::from_u32(buf) {
            Some(c)
        } else {
            // 2 bytes char
            buf |= (self.bytes.next().expect("token error").expect("io error") as u32) << 8;
            if let Some(c) = char::from_u32(buf) {
                Some(c)
            } else {
                // 3 bytes char
                buf |= (self.bytes.next().expect("token error").expect("io error") as u32) << 8;
                if let Some(c) = char::from_u32(buf) {
                    Some(c)
                } else {
                    // 4 bytes char
                    buf |= (self.bytes.next().expect("token error").expect("io error") as u32) << 8;
                    Some(char::from_u32(buf).expect("token error"))
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn char_test() {
        let a = 0x10a52u32;
        println!("{:?}", char::from_u32(a));
    }
}