use super::{
    char_gen::EncodeError,
    word_gen::{self, Word, WordGeneratorTrait, WordKind},
    Error,
};

// トークン型のvariantの型は要検討
// 型の候補↓
// Wordのvariantの型: String, Vec<char>
// Oprのvariantの型: String, ShortVec<char, 3>, [char; 3]など
/// トークン型
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    // token kinds
    /// 整数
    Integer(i64),
    /// 識別子、キーワードなど
    Ident(Box<str>),
    /// 演算子
    Opr([Option<char>; 3]),
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Self::Integer(_) => TokenKind::Integer,
            Self::Ident(_) => TokenKind::Ident,
            Self::Opr(_) => TokenKind::Opr,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Integer,
    Ident,
    Opr,
}

impl Token {
    pub fn opr(s: &str) -> Self {
        let mut s = s.chars();
        Self::Opr([s.next(), s.next(), s.next()])
    }

    #[inline]
    pub fn single_operator(c: char) -> Self {
        Self::Opr([Some(c), None, None])
    }
}

impl From<Token> for String {
    fn from(value: Token) -> Self {
        match value {
            Token::Integer(n) => n.to_string(),
            Token::Opr(s) => s.iter().flatten().collect(),
            Token::Ident(s) => s.into(),
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Integer(n) => n.to_string(),
            Token::Opr(s) => s.iter().flatten().collect(),
            Token::Ident(s) => s.to_string(),
        }
    }
}

fn report_broken_impl(methods: &[&'static str]) -> ! {
    panic!(
        "`TokenGeneratorTrait` implementation is broken,
        consider checking the following methods:
        {:?}",
        methods
    )
}
/// TokenGeneratorの機能を分離したトレイト
pub trait TokenGeneratorTrait: WordGeneratorTrait {
    /// 次のトークンを返す。
    ///
    /// 先頭の空白文字はスキップされる
    #[inline]
    fn next_token(&mut self) -> Option<Result<Token, Error>> {
        self.skip_whitespaces()
            .next_token_or_space()
            .map(|a| a.map(|a| a.unwrap_or_else(|| report_broken_impl(&["next_token"]))))
    }

    /// スペースが来た場合は読み進めず、`Some(Ok(None))`を返す
    #[inline]
    fn next_token_or_space(&mut self) -> Option<Result<Option<Token>, Error>> {
        Some(match self.next_word_kind()? {
            Err(_) => {
                self.next_char();
                Err(Error::encode_error())
            }
            Ok(WordKind::IdentDigit) => {
                let Some(Ok(c)) = self.peek() else {
                    word_gen::error_next_word_kind()
                };
                if c.is_ascii_digit() {
                    self.parse_next_number().map(Token::Integer).map(Some)
                } else {
                    let Some(Ok(Word::IdentDigit(w))) = self.next_word() else {
                        word_gen::error_next_word_kind()
                    };
                    Ok(Some(Token::Ident(w)))
                }
            }
            Ok(WordKind::Separator) => {
                let Some(Ok(Word::Separator(separator))) = self.next_word() else {
                    word_gen::error_next_word_kind()
                };
                Ok(Some(Token::single_operator(separator)))
            }
            Ok(WordKind::Spaces | WordKind::NewLine) => Ok(None),
        })
    }

    fn token_iter(self) -> impl Iterator<Item = Result<Token, Error>>
    where
        Self: Sized,
    {
        Iter(self)
    }

    /// 先頭の空白文字を読み進めない
    ///
    /// 空白文字を読み進めたい場合は
    /// `self.skip_whitespaces().consume_identifier()`
    /// とする
    #[inline]
    fn consume_identifier(&mut self) -> Option<Box<str>> {
        match self.predict() {
            Some(Ok(Some(TokenKind::Ident))) => match self.next_word() {
                Some(Ok(Word::IdentDigit(s))) => Some(s),
                _ => panic!("next_word() returned non-word token when peeked word."),
            },
            _ => None,
        }
    }

    /// 先頭の空白文字を読み進めない
    #[inline]
    fn consume_operator(&mut self, op: char) -> bool {
        debug_assert!(self.is_separator(op));
        self.consume(op)
    }

    /// 正確に次のトークンを予測するために空白文字は読み進められる
    #[inline]
    fn next_token_kind(&mut self) -> Option<Result<TokenKind, EncodeError>> {
        self.skip_whitespaces();
        self.predict().map(|a| a.map(Option::unwrap))
    }

    /// 先頭の空白文字を読み進めない
    #[inline]
    fn ignore_token(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.predict() == Some(Ok(Some(kind)))
            && self
                .next_token()
                .unwrap_or_else(|| {
                    panic!("TokenGenerator returned EOF but some charactors remain.")
                })?
                .kind()
                != kind
        {
            panic!("TokenGenerator returned different token kind when peeked.");
        }
        Ok(())
    }

    /// トークンの種類を予測する。
    ///
    /// 空白文字が次に来る場合は`Some(Ok(None))`を返す
    #[inline]
    fn predict(&self) -> Option<Result<Option<TokenKind>, EncodeError>> {
        Some(match self.peek()? {
            Ok(c) if c.is_ascii_whitespace() => Ok(None),
            Ok(c) if c.is_ascii_digit() => Ok(Some(TokenKind::Integer)),
            Ok(c) if self.is_separator(c) => Ok(Some(TokenKind::Opr)),
            Ok(_) => Ok(Some(TokenKind::Ident)),
            Err(_) => Err(EncodeError),
        })
    }
}

trait TokenGeneratorPrivate: TokenGeneratorTrait {
    /// 0x,0bなどの接頭辞を含めて、次に来る数字を読み取る
    #[inline]
    fn parse_next_number(&mut self) -> Result<i64, Error> {
        let radix = if self.consume('0') {
            if self.consume_with(|c| matches!(c, 'x' | 'X')) {
                16
            } else if self.consume_with(|c| matches!(c, 'o' | 'O')) {
                8
            } else if self.consume_with(|c| matches!(c, 'b' | 'B')) {
                2
            } else if self.consume_with(|c| c.is_ascii_digit()) {
                10
            } else if matches!(self.next_word_kind(), Some(Ok(WordKind::IdentDigit))) {
                let Some(Ok(c)) = self.next_char() else {
                    report_broken_impl(&["parse_next_number"])
                };
                self.ignore_word_of(WordKind::IdentDigit);
                return Err(Error::new_int_prefix_error(
                    format!("0{c}").into_boxed_str(),
                ));
            } else {
                return Ok(0);
            }
        } else {
            10
        };
        self.parse_next_number_as(radix)
    }

    /// 次に数字が来ることが分かっているときに、その数字を指定した進数で読み取る。
    ///
    /// パース不可能な文字列を読み取った場合は`ParseIntError`を返す。
    #[inline]
    fn parse_next_number_as(&mut self, radix: u32) -> Result<i64, Error> {
        let Some(Ok(Word::IdentDigit(w))) = self.next_word() else {
            report_broken_impl(&["parse_next_number_as"])
        };
        i64::from_str_radix(&w, radix).map_err(|_| Error::new_parse_int_error(w, radix))
    }
}

impl<T: TokenGeneratorTrait + ?Sized> TokenGeneratorPrivate for T {}

impl<T: WordGeneratorTrait + ?Sized> TokenGeneratorTrait for T {}

struct Iter<I>(I);

impl<I: TokenGeneratorTrait> Iterator for Iter<I> {
    type Item = Result<Token, Error>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::super::char_gen;
    use super::*;

    #[test]
    fn token_generator_test() {
        // 具体的なケースを入れてみる
        let s: &[u8] = b"#general_definition\n*word_size\t32\n#end\n#rule_definition\nadd 0xFF, opr(0:4), opr(1:4)\n#end";
        let binding = char_gen::CharGenerator::new(s);
        let mut gen = binding.into_word_generator().token_iter();
        assert_eq!(gen.next(), Some(Ok(Token::opr("#"))));
        assert_eq!(
            gen.next(),
            Some(Ok(Token::Ident("general_definition".into())))
        );
        assert_eq!(gen.next(), Some(Ok(Token::opr("*"))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("word_size".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(32))));
        assert_eq!(gen.next(), Some(Ok(Token::opr("#"))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("end".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::opr("#"))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("rule_definition".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("add".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(255))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(","))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("opr".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::opr("("))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(")"))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(","))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("opr".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::opr("("))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(1))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(")"))));
        assert_eq!(gen.next(), Some(Ok(Token::opr("#"))));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("end".into()))));
        assert_eq!(gen.next(), None);

        // 各種進数指定
        let s: &[u8] = b"10 0x0f 0o17 0b1010 0xG 0a12 0012 0:4 0\n4";
        let binding = char_gen::CharGenerator::new(s);
        let mut gen = binding.into_word_generator().token_iter();
        assert_eq!(gen.next(), Some(Ok(Token::Integer(10))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0x0f))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0o17))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0b1010))));
        assert_eq!(
            gen.next(),
            Some(Err(Error::new_parse_int_error("G".into(), 16)))
        );
        assert_eq!(
            gen.next(),
            Some(Err(Error::new_int_prefix_error("0a".into())))
        );
        assert_eq!(gen.next(), Some(Ok(Token::Integer(12))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
        assert_eq!(gen.next(), None);

        // 余計なwhitespaceを無視できるか
        let s: &[u8] = b"\t \t a   \t\t : \n    123";
        let binding = char_gen::CharGenerator::new(s);
        let mut gen = binding.into_word_generator().token_iter();
        assert_eq!(gen.next(), Some(Ok(Token::Ident("a".into()))));
        assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(123))));
        assert_eq!(gen.next(), None);

        // encode error
        let s = [
            0xff, b'1', 0xff, b'0', 0xff, b'a', 0xff, b'\n', 0xff, b']', 0xff, b' ', 0xff,
        ];
        let biding = char_gen::CharGenerator::new(&s[..]);
        let mut gen = biding.into_word_generator().token_iter();
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(1))));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Ok(Token::Ident("a".into()))));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Ok(Token::opr("]"))));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), Some(Err(Error::encode_error())));
        assert_eq!(gen.next(), None);
    }

    #[test]
    fn token_generator_util_test() {
        let code: &[u8] = b"#general_definition\n*word_size\t32\n#end\n#rule_definition\nadd 0xFF, opr(0:4), opr(1:4)\n#end";
        let chars = char_gen::CharGenerator::new(code);
        let mut gen = chars.into_word_generator();
        assert!(gen.consume_identifier().is_none());
        assert!(gen.consume_operator('#'));
        assert_eq!(gen.consume_identifier(), Some("general_definition".into()));
        assert!(!gen.consume_operator('*'));
        assert_eq!(gen.predict(), Some(Ok(None)));
        assert_eq!(gen.next_word_kind(), Some(Ok(WordKind::NewLine)));
        assert_eq!(gen.next_token_kind(), Some(Ok(TokenKind::Opr)));
        assert_eq!(
            gen.next_token_or_space(),
            Some(Ok(Some(Token::single_operator('*'))))
        );
        assert_eq!(gen.next_token_kind(), Some(Ok(TokenKind::Ident)));
        assert_eq!(gen.consume_identifier(), Some("word_size".into()));
        assert_eq!(gen.next_token_kind(), Some(Ok(TokenKind::Integer)));
        assert_eq!(
            gen.next_token_or_space(),
            Some(Ok(Some(Token::Integer(32))))
        );
    }
}
