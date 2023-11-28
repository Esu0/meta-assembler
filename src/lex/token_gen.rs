use std::io;

use super::char_gen::CharGenerator;

/// 演算子のリスト
const OPERATORS: [&str; 8] = ["#", "*", ";", "(", ")", "[", "]", ":"];

// トークン型のvariantの型は要検討
// 型の候補↓
// Wordのvariantの型: String, Vec<char>
// Oprのvariantの型: String, ShortVec<char, 3>, [char; 3]など
/// トークン型
#[derive(Debug)]
pub enum Token {
    // token kinds
    /// 整数
    Integer(u64),
    /// 識別子、キーワードなど
    Word(String),
    /// 演算子
    Opr(String),
}

/// トークンを生成するイテレータ
#[derive(Clone, Debug)]
pub struct TokenGenerator<C> {
    chars: CharGenerator<C>,
    // additional field
}

impl<C: Iterator<Item = io::Result<char>>> TokenGenerator<C> {
    pub fn new(chars: CharGenerator<C>) -> Self {
        Self { chars }
    }

    // parserから位置情報が見えない問題を解決するために、
    // 内部の参照にアクセスできるようにする。
    /// 内部の文字のイテレータを返す
    pub fn inner(&self) -> &CharGenerator<C> {
        &self.chars
    }
}

impl<C: Iterator<Item = io::Result<char>>> Iterator for TokenGenerator<C> {
    type Item = Result<Token, super::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(r) = self.chars.next() {
            match r {
                Ok(c) => Some(Ok(Token::Word(c.to_string()))),
                Err(e) => {
                    match e.kind() {
                        io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                            Some(Err(
                                super::Error {
                                    inner: super::_Error::EncodeError { line: 1, column: 1 }
                                }
                            ))
                        },
                        _ => unreachable!(),
                    }
                },
            }
        } else {
            None
        }
    }
}

#[test]
fn token_generator_test() {
    let buf = [0xffu8, 0xffu8];
    let mut binding = super::char_gen::CharGenerator::new(buf.as_ref());
    let mut gen = TokenGenerator::new(binding.chars());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
}