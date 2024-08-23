use flatbuffers_codegen::parser::{
    collect_includes, get_namespaced_decls, parse_file, ParserState,
};

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

    let namespaced_decls = get_namespaced_decls(&file).unwrap();

    let mut state = ParserState::new();
    state.extend_decls(namespaced_decls);

    let items = parse_file(&file, &state).unwrap();

    println!("Items found:");
    println!("{items:#?}");

    println!("State:");
    println!("{state:#?}");
}
