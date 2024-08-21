use flatbuffers_codegen::parser::parse_file;

fn main() {
    let file = std::fs::read_to_string("../examples/helloworld/fbs/service2.fbs").unwrap();

    let items = parse_file(&file).unwrap();

    println!("Items found:");
    println!("{items:#?}");
}
