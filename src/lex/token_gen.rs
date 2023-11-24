// const OPERATION_MAP: &'static [(&'static str, Token)] = &[("+", Token::Add)];

pub enum Token {
    // token kinds
    Add,
    Value(i32),
}


#[derive(Clone, Debug)]
pub struct TokenGenerator<C> {
    chars: C,
    // additional field
}

impl<C: Iterator<Item = char>> TokenGenerator<C> {
    pub fn new(chars: C) -> Self {
        Self { chars }
    }
}

impl<C: Iterator<Item = char>> Iterator for TokenGenerator<C> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}
