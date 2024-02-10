use std::{fs::File, io::BufReader};

use meta_assembler::{lex::char_gen::CharGenerator, parser::cgr::Parser};

fn main() {
    let file = File::open("PICOM8.cgr").unwrap();
    let reader = BufReader::new(file);
    let char_gen = CharGenerator::new(reader);
    let token_gen = char_gen.into_word_generator();
    let parser = Parser::new(token_gen);
    let rule = parser.compile().unwrap();
    println!("{:#?}", rule);
}