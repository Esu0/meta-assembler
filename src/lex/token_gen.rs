use std::io;

use super::char_gen::CharGenerator;

/// 演算子のリスト
const OPERATORS: [char; 8] = ['#', '*', ';', '(', ')', '[', ']', ':'];

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
    /// 改行
    NewLine,
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
        return !Self::is_opr(c) && !Self::is_whitespace(c) && !Self::is_newline(c);
    }
}

/// トークンを生成するイテレータ
#[derive(Clone, Debug)]
pub struct TokenGenerator<C> {
    chars: CharGenerator<C>,
    // additional field
    next_head: Option<char>,
}

impl<C: Iterator<Item = io::Result<char>>> TokenGenerator<C> {
    pub fn new(chars: CharGenerator<C>) -> Self {
        Self { chars, next_head: None }
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
        let head = match self.next_head {
            Some(h) => Ok(h),
            None => self.chars.next()?
        };
        if let Err(e) = head {
            match e.kind() {
                io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                    let (line, column) = self.chars.position();
                    return Some(Err(super::Error::new_encode_error(line, column)));
                },
                _ => unreachable!(),
            }
        }
        let mut head = head.unwrap();
        // オペランドまたは改行の場合
        if Token::is_opr(head) {
            self.next_head = None;
            return Some(Ok(Token::Opr(head.to_string())));
        }
        if Token::is_newline(head) {
            self.next_head = None;
            return Some(Ok(Token::NewLine));
        }
        // white space をすべて無視し、Tokenの先頭を取得
        while Token::is_whitespace(head) {
            let elem = self.chars.next()?;
            if let Err(e) = elem {
                match e.kind() {
                    io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                        self.next_head = None;
                        let (line, column) = self.chars.position();
                        return Some(Err(super::Error::new_encode_error(line, column)));
                    },
                    _ => unreachable!(),
                }
            }
            head = elem.unwrap();
        }
        // トークンの先頭が数字でないとき
        if !head.is_digit(10) {
            let mut ret = head.to_string();
            loop {
                let elem = self.chars.next();
                match elem {
                    Some(Err(e)) => {
                        match e.kind() {
                            io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                                self.next_head = None;
                                let (line, column) = self.chars.position();
                                return Some(Err(super::Error::new_encode_error(line, column)));
                            },
                            _ => unreachable!(),
                        }
                    },
                    Some(Ok(c)) => {
                        if Token::is_common(c) {
                            ret.push(c);
                        } else {
                            self.next_head = Some(c);
                            return Some(Ok(Token::Word(ret)));
                        }
                    },
                    None => {
                        self.next_head = None;
                        return Some(Ok(Token::Word(ret)));
                    }
                }
            }
        }
        // トークンの先頭が0の時
        let mut radix = 10;
        let mut ret = String::new();
        if head == '0' {
            let elem = self.chars.next()?;
            if let Err(e) = elem {
                match e.kind() {
                    io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                        self.next_head = None;
                        let (line, column) = self.chars.position();
                        return Some(Err(super::Error::new_encode_error(line, column)));
                    },
                    _ => unreachable!(),
                }
            }
            let c = elem.unwrap();
            match c {
                'x' => radix = 16,
                'o' => radix = 8,
                'b' => radix = 2,
                _ => {
                    if c.is_digit(10) {
                        // 0から始まる数字
                        ret.push(head);
                        ret.push(c);
                    } else {
                        self.next_head = Some(c);
                        return Some(Ok(Token::Integer(0)));
                    }
                }
            }
        } else {
            ret.push(head);
        }
        loop {
            let elem = self.chars.next()?;
            if let Err(e) = elem {
                match e.kind() {
                    io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                        self.next_head = None;
                        let (line, column) = self.chars.position();
                        return Some(Err(super::Error::new_encode_error(line, column)));
                    },
                    _ => unreachable!(),
                }
            }
            let c = elem.unwrap();
            if Token::is_common(c) {
                ret.push(c);
            } else {
                self.next_head = Some(c);
                break;
            }
        }
        match u64::from_str_radix(ret.as_str(), radix) {
            Ok(v) => Some(Ok(Token::Integer(v))),
            Err(_) => {
                let (line, column) = self.chars.position();
                Some(Err(super::Error::new_parse_int_error(line, column, ret)))
            }
        }
    }
}

#[test]
fn token_generator_test() {
    let buf = b"\t12aasdf \t  as\ndf\t hjkl      \t";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_token_generator();
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
    println!("{:?}", gen.next());
}