use flatbuffers_codegen::{parser::parse_file, utils::whitespace_and_comments_opt};
use winnow::Parser;

fn main() {
    let input = "";
    let res = whitespace_and_comments_opt.parse(input).unwrap();
    println!("{res:?}");

    let file = std::fs::read_to_string("../examples/helloworld/fbs/service2.fbs").unwrap();

    let items = parse_file(&file).unwrap();

    println!("Items found:");
    println!("{items:#?}");
}
