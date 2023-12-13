use std::io;

use super::{char_gen::CharGenerator, word_gen::WordGeneratorTrait};

/// 演算子のリスト
/// 
/// 代わりにSeparatorを使うので、後で削除
const OPERATORS: [char; 9] = ['#', '*', ';', '(', ')', '[', ']', ':', ','];

// トークン型のvariantの型は要検討
// 型の候補↓
// Wordのvariantの型: String, Vec<char>
// Oprのvariantの型: String, ShortVec<char, 3>, [char; 3]など
/// トークン型
#[derive(Debug, PartialEq)]
pub enum Token {
    // token kinds
    /// 整数
    Integer(u64),
    /// 識別子、キーワードなど
    Word(String),
    /// 演算子
    Opr(String),
    /// 改行
    NewLine,
}

impl From<Token> for String {
    fn from(value: Token) -> Self {
        match value {
            Token::Integer(n) => n.to_string(),
            Token::Opr(s) => s,
            Token::Word(s) => s,
            Token::NewLine => "\\n".to_string(),
        }
    }
}

impl Token {
    fn is_opr(c: char) -> bool {
        OPERATORS.contains(&c)
    }
    fn is_newline(c: char) -> bool {
        c == '\n'
    }
    fn is_whitespace(c: char) -> bool {
        !Self::is_newline(c) && c.is_whitespace()
    }
    // WordまたはIntegerの一部
    fn is_common(c: char) -> bool {
        !Self::is_opr(c) && !Self::is_whitespace(c) && !Self::is_newline(c)
    }
}

/// charのイテレータを受け取ってトークンのイテレータにする
#[derive(Clone, Debug)]
pub struct TokenGenerator<C> {
    chars: CharGenerator<C>,
    // additional field
    next_head: Option<Result<char, super::Error>>,
}

impl<C: Iterator<Item = io::Result<char>>> TokenGenerator<C> {
    pub fn new(chars: CharGenerator<C>) -> Self {
        Self {
            chars,
            next_head: None,
        }
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
        // 一文字目を取得
        let mut head = match self.next_head.clone() {
            // 一文字目が前回の実行で取得されている場合
            Some(Ok(h)) => h,
            // 前回のエンコードでエラーが起きている場合
            Some(Err(e)) => {
                self.next_head = None;
                return Some(Err(e.clone()));
            }
            // 一文字目が前回の実行で取得されていない場合
            None => match self.chars.next()? {
                Ok(h) => h,
                Err(_) => {
                    // エラーは io::ErrorKind::InvalidData or io::ErrorKind::UnexpectedEof
                    self.next_head = None;
                    let (line, column) = self.chars.position();
                    return Some(Err(super::Error::new_encode_error(line, column)));
                }
            },
        };
        // white space をすべて無視し、Tokenの先頭を取得
        while Token::is_whitespace(head) {
            head = match self.chars.next()? {
                Ok(h) => h,
                Err(_) => {
                    self.next_head = None;
                    let (line, column) = self.chars.position();
                    return Some(Err(super::Error::new_encode_error(line, column)));
                }
            }
        }
        let (line, column) = self.chars.position();
        // オペランドまたは改行の場合
        if Token::is_opr(head) {
            self.next_head = None;
            return Some(Ok(Token::Opr(head.to_string())));
        }
        if Token::is_newline(head) {
            self.next_head = None;
            return Some(Ok(Token::NewLine));
        }

        let mut radix = 10;
        let mut ret = String::new();
        // トークンの先頭が数字でないとき
        if !head.is_ascii_digit() {
            // 先頭の文字をretに追加
            ret.push(head);
            loop {
                // Token::is_common()がtrueである間、retに文字を追加
                match self.chars.next() {
                    Some(Ok(c)) => {
                        if Token::is_common(c) {
                            ret.push(c);
                        } else {
                            // cは演算子or改行or空白
                            self.next_head = Some(Ok(c));
                            break;
                        }
                    }
                    Some(Err(_)) => {
                        self.next_head = Some(Err(super::Error::new_encode_error(line, column)));
                        break;
                    }
                    None => {
                        self.next_head = None;
                        break;
                    }
                }
            }
            return Some(Ok(Token::Word(ret)));
        }
        // トークンの先頭が数字で、'0'の場合
        // 進数指定の可能性を検討
        if head == '0' {
            match self.chars.next() {
                Some(Ok(c)) => {
                    match c {
                        // 進数指定する接頭辞の場合
                        'x' => radix = 16,
                        'o' => radix = 8,
                        'b' => radix = 2,
                        _ => {
                            if Token::is_common(c) {
                                // 進数指定ではない場合
                                // 0から始まる10進数の数字
                                ret.push(head);
                                // cが数字かは、後で判定する
                                ret.push(c);
                            } else {
                                // cは演算子or改行or空白
                                self.next_head = Some(Ok(c));
                                return Some(Ok(Token::Integer(0)));
                            }
                        }
                    }
                }
                Some(Err(_)) => {
                    self.next_head = Some(Err(super::Error::new_encode_error(line, column)));
                    return Some(Ok(Token::Integer(0)));
                }
                None => {
                    // ファイルの末尾にたどりついた
                    self.next_head = None;
                    return Some(Ok(Token::Integer(0)));
                }
            }
        } else {
            ret.push(head);
        }
        // トークンの先頭が数字のとき
        loop {
            match self.chars.next() {
                Some(Ok(c)) => {
                    if Token::is_common(c) {
                        ret.push(c);
                    } else {
                        self.next_head = Some(Ok(c));
                        break;
                    }
                }
                Some(Err(_)) => {
                    self.next_head = Some(Err(super::Error::new_encode_error(line, column)));
                    break;
                }
                None => {
                    self.next_head = None;
                    break;
                }
            }
        }
        match u64::from_str_radix(ret.as_str(), radix) {
            Ok(v) => Some(Ok(Token::Integer(v))),
            Err(_) => match radix {
                16 => Some(Err(super::Error::new_parse_int_error(
                    line,
                    column,
                    "0x".to_string() + ret.as_str(),
                ))),
                10 => Some(Err(super::Error::new_parse_int_error(line, column, ret))),
                8 => Some(Err(super::Error::new_parse_int_error(
                    line,
                    column,
                    "0o".to_string() + ret.as_str(),
                ))),
                2 => Some(Err(super::Error::new_parse_int_error(
                    line,
                    column,
                    "0b".to_string() + ret.as_str(),
                ))),
                _ => unreachable!(),
            },
        }
    }
}

/// TokenGeneratorの機能を分離したトレイト
pub trait TokenGeneratorTrait: WordGeneratorTrait {
    /// 次のトークンを返す。
    /// OperatorはSeparatorと同じでよい。
    fn next_token(&mut self) -> Option<Result<Token, super::Error>> {
        todo!("implement next_token")
    }

    /// 次のトークンが純粋な数字の列ならば、それを文字列として返す。
    ///
    /// それ以外のトークンならば、`Ok(Err(Some(Token)))`、EOFなら`Ok(Err(None))`を返す。
    fn expect_number_as_string(&mut self) -> Result<Result<Box<str>, Option<Token>>, super::Error> {
        todo!("implement expect_number_as_string")
    }

    fn read_binary_number(&mut self) -> Result<Result<u64, Option<Token>>, super::Error> {
        let s = self.expect_number_as_string()?;
        match s {
            Ok(s) => Ok(Ok(s.parse().map_err(|_| {
                let (l, c) = self.reader_position();
                super::Error::new_parse_int_error(l, c, s.into())
            })?)),
            Err(t) => Ok(Err(t)),
        }
    }
}

#[test]
fn token_generator_test() {
    // 具体的なケースを入れてみる
    let buf = b"#general_definition\n*word_size\t32\n#end\n#rule_definition\nadd 0xFF, opr(0:4), opr(1:4)\n#end";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_token_generator();
    assert_eq!(gen.next(), Some(Ok(Token::Opr("#".to_string()))));
    assert_eq!(
        gen.next(),
        Some(Ok(Token::Word("general_definition".to_string())))
    );
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("*".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("word_size".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(32))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("#".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("end".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("#".to_string()))));
    assert_eq!(
        gen.next(),
        Some(Ok(Token::Word("rule_definition".to_string())))
    );
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Word("add".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(255))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(",".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("opr".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("(".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(":".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(")".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(",".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("opr".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("(".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(1))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(":".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(")".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("#".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("end".to_string()))));
    assert_eq!(gen.next(), None);

    // 各種進数指定
    let buf = b"10 0x0f 0o17 0b1010 0xG 0a12 0012 0:4 0\n4";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_token_generator();
    assert_eq!(gen.next(), Some(Ok(Token::Integer(10))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0x0f))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0o17))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0b1010))));
    assert_eq!(
        gen.next(),
        Some(Err(super::Error::new_parse_int_error(
            1,
            22,
            "0xG".to_string()
        )))
    );
    assert_eq!(
        gen.next(),
        Some(Err(super::Error::new_parse_int_error(
            1,
            26,
            "0a12".to_string()
        )))
    );
    assert_eq!(gen.next(), Some(Ok(Token::Integer(12))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(":".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), None);

    // 余計なwhitespaceを無視できるか
    let buf = b"\t \t a   \t\t : \n    123";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_token_generator();
    assert_eq!(gen.next(), Some(Ok(Token::Word("a".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr(":".to_string()))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(123))));
    assert_eq!(gen.next(), None);

    // encode error
    let buf = [
        0xff, b'1', 0xff, b'0', 0xff, b'a', 0xff, b'\n', 0xff, b']', 0xff, b' ', 0xff,
    ];
    let mut p = buf.as_ref();
    let biding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = biding.into_token_generator();
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(1, 1))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(1))));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(1, 2))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(1, 3))));
    assert_eq!(gen.next(), Some(Ok(Token::Word("a".to_string()))));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(1, 4))));
    assert_eq!(gen.next(), Some(Ok(Token::NewLine)));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(2, 1))));
    assert_eq!(gen.next(), Some(Ok(Token::Opr("]".to_string()))));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(2, 2))));
    assert_eq!(gen.next(), Some(Err(super::Error::new_encode_error(2, 3))));
    assert_eq!(gen.next(), None);
}
