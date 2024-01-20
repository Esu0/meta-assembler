use super::{word_gen::{WordGeneratorTrait, Word, WordKind, self}, Error};

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
    /// OperatorはSeparatorと同じでよい。
    fn next_token(&mut self) -> Option<Result<Token, Error>> {
        self.skip_whitespaces();
        Some(match self.next_word_kind()? {
            Err(_) => {
                self.next_char();
                Err(Error::encode_error())
            },
            Ok(WordKind::Word) => {
                let Some(Ok(c)) = self.peek() else {
                    word_gen::error_next_word_kind()
                };
                if c.is_ascii_digit() {
                    self.parse_next_number().map(Token::Integer)
                } else {
                    let Some(Ok(Word::Word(w))) = self.next_word() else {
                        word_gen::error_next_word_kind()
                    };
                    Ok(Token::Ident(w))
                }
            },
            Ok(WordKind::Separator) => {
                let Some(Ok(Word::Separator(separator))) = self.next_word() else {
                    word_gen::error_next_word_kind()
                };
                Ok(Token::single_operator(separator))
            }
            _ => {
                word_gen::report_broken_impl(&["next_word_kind", "skip_whitespaces"]);
            }
        })
    }

    /// 次のトークンが純粋な数字の列ならば、それを文字列として返す。
    ///
    /// それ以外のトークンならば、`Ok(Err(Some(Token)))`、EOFなら`Ok(Err(None))`を返す。
    fn expect_number_as_string(&mut self) -> Result<Result<Box<str>, Option<Token>>, Error> {
        todo!("expect_number_as_string")
    }

    fn read_binary_number(&mut self) -> Result<Result<u64, Option<Token>>, Error> {
        let s = self.expect_number_as_string()?;
        match s {
            Ok(s) => Ok(Ok(s
                .parse()
                .map_err(|_| Error::new_parse_int_error(s))?)),
            Err(t) => Ok(Err(t)),
        }
    }

    fn token_iter(self) -> impl Iterator<Item = Result<Token, Error>>
    where
        Self: Sized,
    {
        Iter(self)
    }
}

trait TokenGeneratorPrivate: TokenGeneratorTrait {
    /// 0x,0bなどの接頭辞を含めて、次に来る数字を読み取る
    fn parse_next_number(&mut self) -> Result<i64, Error> {
        let radix = if self.consume('0') {
            if self.consume_with(|c| matches!(c, 'x' | 'X')) {
                16
            } else if self.consume_with(|c| matches!(c, 'o' | 'O')) {
                8
            } else if self.consume_with(|c| matches!(c, 'b' | 'B')) {
                2
            } else {
                10
            }
        } else {
            10
        };
        self.parse_next_number_as(radix)
    }

    /// 次に数字が来ることが分かっているときに、その数字を指定した進数で読み取る。
    ///
    /// パース不可能な文字列を読み取った場合は`ParseIntError`を返す。
    fn parse_next_number_as(&mut self, radix: u32) -> Result<i64, Error> {
        let Some(Ok(Word::Word(w))) = self.next_word() else {
            report_broken_impl(&["WordGenerator::next_word", "WordGenerator::next_word_kind"])
        };
        i64::from_str_radix(&w, radix).map_err(|_| Error::new_parse_int_error(w))
    }
}

impl<T: TokenGeneratorTrait + ?Sized> TokenGeneratorPrivate for T {}

impl<T: WordGeneratorTrait + ?Sized> TokenGeneratorTrait for T {}

struct Iter<I>(I);

impl<I: TokenGeneratorTrait> Iterator for Iter<I> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_token()
    }
}

#[test]
fn token_generator_test() {
    // 具体的なケースを入れてみる
    let buf = b"#general_definition\n*word_size\t32\n#end\n#rule_definition\nadd 0xFF, opr(0:4), opr(1:4)\n#end";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
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
    assert_eq!(
        gen.next(),
        Some(Ok(Token::Ident("rule_definition".into())))
    );
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
    let buf = b"10 0x0f 0o17 0b1010 0xG 0a12 0012 0:4 0\n4";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_word_generator().token_iter();
    assert_eq!(gen.next(), Some(Ok(Token::Integer(10))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0x0f))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0o17))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0b1010))));
    assert_eq!(
        gen.next(),
        Some(Err(Error::new_parse_int_error("0xG".into())))
    );
    assert_eq!(
        gen.next(),
        Some(Err(Error::new_parse_int_error("0a12".into())))
    );
    assert_eq!(gen.next(), Some(Ok(Token::Integer(12))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(0))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(4))));
    assert_eq!(gen.next(), None);

    // 余計なwhitespaceを無視できるか
    let buf = b"\t \t a   \t\t : \n    123";
    let mut p = buf.as_ref();
    let binding = super::char_gen::CharGenerator::new(&mut p);
    let mut gen = binding.into_word_generator().token_iter();
    assert_eq!(gen.next(), Some(Ok(Token::Ident("a".into()))));
    assert_eq!(gen.next(), Some(Ok(Token::opr(":"))));
    assert_eq!(gen.next(), Some(Ok(Token::Integer(123))));
    assert_eq!(gen.next(), None);

    // encode error
    let buf = [
        0xff, b'1', 0xff, b'0', 0xff, b'a', 0xff, b'\n', 0xff, b']', 0xff, b' ', 0xff,
    ];
    let mut p = buf.as_ref();
    let biding = super::char_gen::CharGenerator::new(&mut p);
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
