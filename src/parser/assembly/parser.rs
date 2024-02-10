use crate::lex::token_gen::TokenGeneratorTrait;

use super::Rules;

// temporary
#[allow(unused)]
pub struct Parser<T: TokenGeneratorTrait> {
    tokens: T,
    rules: Rules,
}
