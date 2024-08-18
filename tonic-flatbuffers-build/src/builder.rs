use std::{
    collections::{HashMap, HashSet},
    io,
    path::{Path, PathBuf},
};

use itertools::Itertools;

macro_rules! io_error {
    ($e:expr) => {
        io::Error::new(io::ErrorKind::Other, $e)
    };
}

macro_rules! ok_or_invalid_service {
    ($x:expr) => {
        $x.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Invalid service definition"))
    };
}

#[derive(Debug)]
struct Endpoint<'a> {
    name: &'a str,
    req: &'a str,
    resp: &'a str,
    streaming: Option<&'a str>,
}

#[derive(Debug)]
struct Service<'a> {
    namespace: &'a str,
    name: &'a str,
    endpoints: Vec<Endpoint<'a>>,
}

impl<'a> Service<'a> {
    pub fn parse(buf: &'a str, namespace: &'a str) -> Result<Self, io::Error> {
        let mut service = Service {
            namespace,
            name: "",
            endpoints: Vec::new(),
        };

        let (_, buf) = ok_or_invalid_service!(buf.split_once(' '))?;

        let buf = service.read_name(buf);

        let mut buf = service.consume_char(buf, '{');

        while buf != "}" {
            let end_idx = ok_or_invalid_service!(buf.find(';'))?;
            service.parse_endpoint(&buf[..=end_idx])?;
            buf = buf[end_idx + 1..].trim();
        }

        Ok(service)
    }

    fn read_name(&mut self, buf: &'a str) -> &'a str {
        let Some(idx) = buf.find('{') else {
            return buf;
        };

        let name = buf[..idx].trim();
        let buf = buf[idx..].trim();

        self.name = name;
        buf
    }

    fn consume_char(&self, buf: &'a str, pat: char) -> &'a str {
        if let Some(idx) = buf.find(pat) {
            buf[idx + 1..].trim()
        } else {
            buf
        }
    }

    fn parse_endpoint(&mut self, buf: &'a str) -> Result<&'a str, io::Error> {
        let Some((input, output)) = buf.split_once(':') else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid endpoint syntax",
            ));
        };

        let input = input.trim();
        let output = output.trim();

        let Some(start_idx) = input.find('(') else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid endpoint syntax",
            ));
        };

        let Some(end_idx) = input.find(')') else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid endpoint syntax",
            ));
        };

        let endpoint_name = &input[..start_idx];
        let input_ident = input[start_idx + 1..end_idx].trim();

        let (buf, output_name, meta) = if let Some(paren_idx) = output.find('(') {
            let name = output[..paren_idx].trim();
            let Some(end_paren_idx) = output.find(')') else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid endpoint syntax",
                ));
            };

            let Some(end_idx) = output.find(';') else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid endpoint syntax",
                ));
            };

            (
                output[end_idx + 1..].trim(),
                name,
                Some(output[paren_idx + 1..end_paren_idx].trim()),
            )
        } else if let Some(end_idx) = output.find(';') {
            let name = output[..end_idx].trim();
            (output[end_idx + 1..].trim(), name, None)
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid endpoint syntax",
            ));
        };

        let meta = meta.map(|meta| {
            let Some((ident, code)) = meta.split_once(':') else {
                return "none";
            };

            let ident = ident.trim();
            let code = code.trim();

            if ident != "streaming" {
                return "none";
            }

            let code = code[1..code.len() - 1].trim();

            code
        });

        self.endpoints.push(Endpoint {
            name: endpoint_name,
            req: input_ident,
            resp: output_name,
            streaming: meta,
        });

        Ok(buf.trim())
    }
}

fn convert_ident_to_owned(ident: &str, namespace: &str) -> String {
    let namespace = if namespace.is_empty() {
        String::new()
    } else {
        format!("{namespace}.")
    };

    let (prefix, ident) = ident.rsplit_once('.').unwrap_or(("", ident));

    if prefix.is_empty() {
        format!("{namespace}Owned{ident}")
    } else {
        format!("{namespace}{prefix}.Owned{ident}")
    }
}

pub fn configure() -> Builder {
    Builder {
        build_client: true,
        build_server: true,
        emit_rerun_if_changed: true,
        generate_default_stubs: false,
        out_dir: None,
    }
}

pub struct Builder {
    pub(crate) build_client: bool,
    pub(crate) build_server: bool,
    pub(crate) emit_rerun_if_changed: bool,
    pub(crate) generate_default_stubs: bool,

    out_dir: Option<PathBuf>,
}

impl Builder {
    /// Enable or disable gRPC client code generation.
    pub fn build_client(mut self, enable: bool) -> Self {
        self.build_client = enable;
        self
    }

    /// Enable or disable gRPC server code generation.
    pub fn build_server(mut self, enable: bool) -> Self {
        self.build_server = enable;
        self
    }

    /// Set the output directory to generate code to.
    ///
    /// Defaults to the `OUT_DIR` environment variable.
    pub fn out_dir(mut self, out_dir: impl AsRef<Path>) -> Self {
        self.out_dir = Some(out_dir.as_ref().to_path_buf());
        self
    }

    /// Enable or disable emitting
    /// [`cargo:rerun-if-changed=PATH`](https://doc.rust-lang.org/cargo/reference/build-scripts.html#rerun-if-changed)
    /// instructions for Cargo.
    ///
    /// If set, writes instructions to `stdout` for Cargo so that it understands
    /// when to rerun the build script. By default, this setting is enabled if
    /// the `CARGO` environment variable is set. The `CARGO` environment
    /// variable is set by Cargo for build scripts. Therefore, this setting
    /// should be enabled automatically when run from a build script. However,
    /// the method of detection is not completely reliable since the `CARGO`
    /// environment variable can have been set by anything else. If writing the
    /// instructions to `stdout` is undesirable, you can disable this setting
    /// explicitly.
    pub fn emit_rerun_if_changed(mut self, enable: bool) -> Self {
        self.emit_rerun_if_changed = enable;
        self
    }

    /// Enable or disable directing service generation to providing a default implementation for service methods.
    /// When this is false all gRPC methods must be explicitly implemented.
    /// When this is true any unimplemented service methods will return 'unimplemented' gRPC error code.
    /// When this is true all streaming server request RPC types explicitly use tonic::codegen::BoxStream type.
    ///
    /// This defaults to `false`.
    pub fn generate_default_stubs(mut self, enable: bool) -> Self {
        self.generate_default_stubs = enable;
        self
    }

    /// Compile the .proto files and execute code generation.
    pub fn compile(
        self,
        fbs: &[impl AsRef<Path>],
        _includes: &[impl AsRef<Path>],
    ) -> io::Result<()> {
        let mut flatbuffers_opts = flatbuffers_build::BuilderOptions::new_with_files(fbs);

        // Set options
        if !self.emit_rerun_if_changed {
            flatbuffers_opts = flatbuffers_opts.supress_buildrs_directives();
        }

        if let Some(path) = self.out_dir.as_ref() {
            flatbuffers_opts = flatbuffers_opts.set_output_path(path);
        }

        let out_dir = self
            .out_dir
            .as_ref()
            .map(|path| {
                path.to_str().map(ToOwned::to_owned).ok_or_else(|| {
                    io::Error::new(io::ErrorKind::Other, "out_dir path is not valid unicode")
                })
            })
            .unwrap_or_else(|| {
                std::env::var("OUT_DIR").map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        "OUT_DIR environment variable is not set",
                    )
                })
            })?;

        // Compile flatbuffers
        flatbuffers_opts
            .compile()
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

        // Convert all flatbuffer RPC definitions into protobuf RPC definitions
        for file in fbs {
            let file = std::fs::read_to_string(file)?.replace(['\r', '\n'], "");

            let mut service_bufs = Vec::new();
            let mut idx = 0;
            while idx < file.len() {
                if let Some(match_idx) = file[idx..].find("rpc_service") {
                    let match_idx = idx + match_idx;
                    let Some(end_idx) = file[match_idx..].find('}') else {
                        break;
                    };

                    // Move the end_idx based off the previous offset
                    let end_idx = match_idx + end_idx;

                    service_bufs.push((match_idx, file[match_idx..=end_idx].trim()));
                    idx = match_idx + 1;
                } else {
                    break;
                }
            }

            let mut fake_types: HashMap<String, HashSet<String>> = HashMap::new();
            let mut file_str = String::new();
            let mut file_namespace = String::new();

            for (start_idx, buf) in service_bufs {
                let namespace = if let Some(ns_idx) = file[..start_idx].rfind("namespace") {
                    let end_idx = ok_or_invalid_service!(file[ns_idx..].find(';'))? + ns_idx;
                    let ns_buf = file[ns_idx..end_idx].replace(['\n', '\r'], "");
                    let (_, namespace) = ok_or_invalid_service!(ns_buf.split_once(' '))?;
                    namespace.to_string()
                } else {
                    String::new()
                };
                file_namespace = namespace.clone();
                let service = Service::parse(buf, &namespace)?;

                file_str.push_str(&format!("service {name} {{", name = service.name));

                for endpoint in service.endpoints.iter() {
                    let input_ident = convert_ident_to_owned(endpoint.req, &namespace);
                    let output_ident = convert_ident_to_owned(endpoint.resp, &namespace);

                    let entry = fake_types.entry(service.namespace.to_string()).or_default();
                    entry.insert(input_ident.clone());
                    entry.insert(output_ident.clone());

                    let input = if let Some(meta) = endpoint.streaming {
                        if meta == "client" || meta == "bidi" {
                            format!("stream {}", input_ident)
                        } else {
                            input_ident
                        }
                    } else {
                        input_ident
                    };

                    let output = if let Some(meta) = endpoint.streaming {
                        if meta == "server" || meta == "bidi" {
                            format!("stream {}", output_ident)
                        } else {
                            output_ident
                        }
                    } else {
                        output_ident
                    };

                    file_str.push_str(&format!(
                        "\n    rpc {name} ({input}) returns ({output});",
                        name = endpoint.name
                    ));
                }

                file_str.push_str("\n}\n")
            }

            let extra_files: HashMap<String, String> = fake_types
                .iter()
                .map(|(namespace, types)| {
                    let mut file_str = format!(
                        r#"
                        syntax = "proto3";
    
                        package {namespace};
                    "#
                    );

                    for typename in types {
                        let (_, typename) = typename.rsplit_once('.').unwrap_or(("", typename));
                        file_str.push_str(&format!("\nmessage {typename} {{}}"));
                    }

                    let filename = format!("fake_{namespace}.proto");

                    (filename, file_str)
                })
                .collect();

            let file_includes = extra_files
                .keys()
                .map(|name| format!("import \"{name}\";"))
                .fold(String::new(), |acc, line| format!("{acc}\n{line}"));

            let file_str = format!(
                r#"
    syntax = "proto3";
    {file_includes}
    
    package {file_namespace};
    
    {file_str}
            "#
            );

            for (filename, content) in extra_files {
                std::fs::write(format!("{out_dir}/{filename}"), content)?;
            }

            std::fs::write(format!("{out_dir}/generated.proto"), file_str)?;

            tonic_build::configure()
                .build_client(true)
                .build_server(true)
                .codec_path("::tonic_flatbuffers::codec::FlatCodec")
                .emit_rerun_if_changed(false)
                .generate_default_stubs(self.generate_default_stubs)
                .out_dir(&out_dir)
                .compile(&[&format!("{out_dir}/generated.proto")], &[&out_dir])?;

            for path in std::fs::read_dir(&out_dir)?.flatten() {
                if let Ok(file_type) = path.file_type() {
                    let Ok(file_name) = path.file_name().into_string() else {
                        continue;
                    };

                    // Remove all protobuf files
                    if file_type.is_file() && file_name.ends_with(".proto") {
                        let _ = std::fs::remove_file(path.path());
                    }
                }
            }

            let mut file = std::fs::read_to_string(&format!("{out_dir}/{file_namespace}.rs"))?;

            let mut owned_list = HashSet::new();

            // Remove generated fake types
            for (_, type_list) in fake_types.iter() {
                for fake_type in type_list {
                    let (_, typename) = fake_type.rsplit_once('.').unwrap_or(("", fake_type));
                    owned_list.insert(typename);
                    let pat = regex::Regex::new(&format!(
                        r"(?:#\[\w+\([\w, =:]+\)\]\n)*pub struct (?:{typename}) \{{\}}\n"
                    ))
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

                    file = pat.replace(&file, "").into_owned();
                }
            }

            let owned_list = owned_list
                .iter()
                .map(|name| name.strip_prefix("Owned").unwrap())
                .join(",");

            let header = "use super::*;";

            file = format!("{header}\n{file}");

            let mut namespace_path = PathBuf::new();
            namespace_path.push(&out_dir);
            namespace_path.extend(file_namespace.split('.'));
            namespace_path.push("grpc.rs");

            std::fs::write(&namespace_path, file)?;
            let _ = std::fs::remove_file(&format!("{out_dir}/{file_namespace}.rs"));

            // Modify the mod file
            let file = std::fs::read_to_string(&format!("{out_dir}/mod.rs"))?;

            let mut mod_def: syn::ItemMod = syn::parse_str(&file).map_err(|e| io_error!(e))?;

            let namespace_segments = file_namespace.split('.').collect_vec();

            if namespace_segments.len() > 1 {
                self.insert_mods(&namespace_segments, 0, &mut mod_def, &owned_list)?;
            } else if let Some((_, items)) = mod_def.content.as_mut() {
                self.add_mods(items, &owned_list)?;
            }

            let new_file = syn::File {
                attrs: vec![],
                items: vec![syn::Item::Mod(mod_def)],
                shebang: None,
            };

            let new_file = prettyplease::unparse(&new_file);

            std::fs::write(&format!("{out_dir}/mod.rs"), new_file)?;
        }

        Ok(())
    }

    fn insert_mods(
        &self,
        segments: &[&str],
        cur_idx: usize,
        item: &mut syn::ItemMod,
        owned_list: &str,
    ) -> Result<(), io::Error> {
        if cur_idx >= segments.len() {
            return Ok(());
        }
        let cur_segment = segments[cur_idx];

        // If this is the last mod, insert stuff here
        if item.ident == cur_segment && cur_idx == segments.len() - 1 {
            if let Some((_, items)) = item.content.as_mut() {
                self.add_mods(items, owned_list)?;
            }

            Ok(())
        } else if item.ident == cur_segment {
            if let Some((_, items)) = item.content.as_mut() {
                for item in items {
                    if let syn::Item::Mod(item) = item {
                        self.insert_mods(segments, cur_idx + 1, item, owned_list)?;
                    }
                }
            }

            Ok(())
        } else {
            return Ok(());
        }
    }

    fn add_mods(&self, items: &mut Vec<syn::Item>, owned_list: &str) -> Result<(), io::Error> {
        let item_mod: syn::ItemMod = syn::parse_str("pub mod grpc;").unwrap();
        let item = syn::Item::Mod(item_mod);
        items.push(item);

        let item_use: syn::ItemUse = syn::parse_str("pub use grpc::*;").unwrap();
        let item = syn::Item::Use(item_use);
        items.push(item);

        if !owned_list.is_empty() {
            let fb_owned_use: syn::ItemUse =
                syn::parse_str("use ::tonic_flatbuffers::flatbuffers_owned::*;").unwrap();
            let item = syn::Item::Use(fb_owned_use);
            items.push(item);

            let macro_def: syn::ItemMacro =
                syn::parse_str(&format!("flatbuffers_owned!({owned_list});"))
                    .map_err(|e| io_error!(e))?;
            let item = syn::Item::Macro(macro_def);
            items.push(item);
        }

        Ok(())
    }
}
