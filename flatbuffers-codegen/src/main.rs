use std::{
    io,
    path::{Path, PathBuf},
};

use flatbuffers_codegen::{generate::FlatbuffersGenerator, parse::{flatbuffers::item::Item, parser::{
    collect_includes, get_namespaced_decls, parse_file, ParserState,
}}};

/// Converts a relative path into absolute
pub fn absolute<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
    let cur_dir = std::env::current_dir()?;

    Ok(path_clean::clean(cur_dir.join(path)))
}

pub fn get_include_paths(file: &str, include_paths: &[PathBuf]) -> anyhow::Result<Vec<PathBuf>> {
    // Get the include declarations from the file
    let includes = collect_includes(file)
        .into_iter()
        .map(|path_str| {
            let path = PathBuf::from(path_str);
            // Import a file on the first matched include, in order. This is how
            // protoc and flatc do it; even if there are multiple files with the
            // same name that resolve the import, only import the first one
            include_paths
                .iter()
                .filter_map(|include_path| {
                    let joined = include_path.join(&path);
                    if std::fs::File::open(&joined).is_ok() {
                        Some(joined)
                    } else {
                        None
                    }
                })
                .next()
                .ok_or_else(|| {
                    anyhow::format_err!(
                        "couldn't locate file \"{path_str}\" in any of the include directories"
                    )
                })
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(includes)
}

struct Dummy;

impl FlatbuffersGenerator for Dummy {}

fn main() {
    // let input = r#"
    //     table Test {
    //         foo:uint32 = 1;
    //         bar:string;
    //     }"#;
    // let state = ParserState::new();

    // let res = parse_file(input, &state).unwrap();
    // println!("{res:?}");

    let include_paths = vec![PathBuf::from("../examples/helloworld/fbs")];

    let files_to_compile = glob::glob("../examples/helloworld/fbs/service.fbs")
        .unwrap()
        .map(|path| {
            let path = path?;
            if path.is_dir() {
                return Err(anyhow::format_err!(
                    "directories are not allowed: {}",
                    path.to_string_lossy()
                ));
            }
            let path = absolute(path)?;
            // Unwrap is safe because it only returns Err if the path ends in `..`, which
            // will never happen because it's an absolute path
            let file_content = std::fs::read_to_string(path)?;
            Ok(file_content)
        })
        .collect::<Result<Vec<_>, anyhow::Error>>()
        .unwrap();

    for file in files_to_compile.iter() {
        let mut state = ParserState::new();
        let cur_includes = get_include_paths(file, &include_paths).unwrap();

        let include_files = cur_includes
            .into_iter()
            .map(std::fs::read_to_string)
            .collect::<Result<Vec<_>, io::Error>>()
            .unwrap();

        let decls = include_files
            .iter()
            .map(|content| get_namespaced_decls(content).map_err(|e| anyhow::format_err!("{e}")));

        for decl in decls {
            let decl = decl.unwrap();
            state.extend_decls(decl);
        }

        // Preprocess this file to get all definitions
        let cur_decls = get_namespaced_decls(file).unwrap();
        state.extend_decls(cur_decls);

        println!("State:");
        println!("{state:#?}");

        let items = parse_file(file, &state).unwrap();

        println!("Items found:");
        println!("{items:#?}");
        
        for item in &items {
            if let Item::Struct(item) = item {
                let tokens = Dummy.generate_struct(item);

                let output = prettyplease::unparse(&syn::parse2(tokens).unwrap());

                println!("Generated impl:");
                println!("{output}");
            }
        }
    }
}
