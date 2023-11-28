use std::io;

use super::char_gen::CharGenerator;

/// 演算子のリスト
const OPERATORS: [&'static str; 8] = ["#", "*", ";", "(", ")", "[", "]", ":"];

// トークン型のvariantの型は要検討
// 型の候補↓
// Wordのvariantの型: String, Vec<char>
// Oprのvariantの型: String, ShortVec<char, 3>, [char; 3]など
/// トークン型
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
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}
