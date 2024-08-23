use flatbuffers_codegen::parser::{parse_file, ParserState};

fn main() {
    let input = r#"
        table Test {
            foo:uint32 = 1;
            bar:string;
        }"#;
    let state = ParserState::new();

    let res = parse_file(input, &state).unwrap();
    println!("{res:?}");

    let file = std::fs::read_to_string("../examples/helloworld/fbs/service2.fbs").unwrap();

    let state = ParserState::new();
    let items = parse_file(&file, &state).unwrap();

    println!("Items found:");
    println!("{items:#?}");
}
