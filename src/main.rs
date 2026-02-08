/**
Rust C Compiler
Jackson Eshbaugh
Built based on the "Writing a C Compiler" book
 */

mod compile_error;
mod lexer;
mod parser;
mod tacky;
mod assembly;
mod driver;

use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} [flags] <input_file.c>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[args.len() - 1];
    let flags = &args[1..args.len() - 1];

    let has = |f: &str| flags.iter().any(|s| s == f);

    let driver_flags = driver::DriverFlags {
        lex: has("--lex"),
        parse: has("--parse"),
        tacky: has("--tacky"),
        codegen: has("--codegen"),
        emit_asm_only: has("-S"),
        keep_temps: has("--keep-temps"),
        verbose: has("--verbose"),
    };

    match driver::compile_file(Path::new(input_file), driver_flags) {
        Ok((stop, paths)) => {
            if matches!(stop, driver::DriverStop::StoppedAfterEmitAsm) {
                println!("Wrote {}", paths.assembly_s.display());
            }
        }
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(1);
        }
    }
}