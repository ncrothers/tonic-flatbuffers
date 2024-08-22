use flatbuffers_codegen::parser::parse_file;

fn main() {
    let input = r#"
        table Test {
            foo:uint32 = 1;
            bar:string = hello;
        }"#;
    let res = parse_file(input).unwrap();
    println!("{res:?}");

    let file = std::fs::read_to_string("../examples/helloworld/fbs/service2.fbs").unwrap();

    let items = parse_file(&file).unwrap();

    println!("Items found:");
    println!("{items:#?}");
}
