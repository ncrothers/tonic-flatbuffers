fn main() -> Result<(), std::io::Error> {
    tonic_flatbuffers_build::configure()
        .emit_rerun_if_changed(true)
        .generate_default_stubs(true)
        .out_dir("src/generated/")
        .compile(&["./fbs/service.fbs"], &["./fbs"])?;

    Ok(())
}
