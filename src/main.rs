/**
Rust C Compiler
Jackson Eshbaugh
Built based on the "Writing a C Compiler" book
 */
mod compile_error;
mod lexer;
mod parser;

use std::collections::VecDeque;
use std::env;
use std::path::Path;
use std::process::Command;
use std::fs;

/**
 This is the compiler driver, which controls the compilation process.
 */
fn main() {
    let args: Vec<String> = env::args().collect();

    // Validate arguments:
    //  1. Ensure an input file has been specified
    if args.len() < 2 {
        eprintln!("Usage: {} [flags] <input_file.c>", args[0]);
        std::process::exit(1);
    }

    //  2. Ensure the given file exists, get its directory.

    let input_file = &args[args.len() - 1];
    let path = Path::new(&input_file);
    let parent_dir = path.parent().expect("Could not find parent directory");
    let program_name = path.file_stem().unwrap().to_string_lossy();

    match path.file_stem() {
        Some(_stem) => (),
        None => {
            eprintln!("No file stem found.");
            std::process::exit(1);
        },
    }

    let flags = &args[1..args.len()-1];

    if !path.exists() {
        eprintln!("{} does not exist.", input_file);
        std::process::exit(1);
    }

    // 1. Preprocess `input_file`

    let preprocess = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&input_file)
        .arg("-o")
        .arg(parent_dir.join(format!("{}.i", program_name.to_string())))
        .output()
        .expect("Failed to run the preprocessor.");

    if !preprocess.status.success() {
        // Something went wrong while preprocessing
        eprintln!("Error running the preprocessor.");
        std::process::exit(1);
    }

    // Preprocessing was successful; begin compilation process

    // 1. LEXER

    let tokens = match lexer::lex(parent_dir.join(format!("{}.i", program_name.to_string())).to_str().unwrap()) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexing failed.\n{}", e);
            std::process::exit(1);
        }
    };


    println!("Lexing completed successfully.");

    for token in tokens.clone() {
        println!("{}", token.kind)
    }

    if flags.contains(&String::from("--lex")) {
        std::process::exit(0);
    }

    // 2. PARSER

    let mut tokens_deque = VecDeque::from(tokens);

    let program = match parser::parse(&mut tokens_deque) {
        Ok(program) => program,
        Err(e) => {
            eprintln!("Parsing failed.\n{}", e);
            std::process::exit(1);
        }
    };

    println!("Parsing complete.");
    println!("Abstract Syntax Tree: \n\n{}", program);

    if flags.contains(&String::from("--parse")) {
        std::process::exit(0);
    }

    // let asm = compiler::codegen(parsed);

    println!("Code generation complete.");

    if flags.contains(&String::from("--codegen")) {
        std::process::exit(0);
    }

    // emit_code(asm, program_name.as_ref());

    match fs::remove_file(parent_dir.join(format!("{}.i", program_name.to_string()))) {
        Ok(_) => (),
        Err(e) => eprintln!("Failed to delete preprocessed.i: {}", e),
    }

    if flags.contains(&String::from("-S")) {
        println!("Wrote {}.s", program_name);
        std::process::exit(0);
    }

    let assemble = Command::new("gcc")
        .arg(parent_dir.join("assembly.s"))
        .arg("-o")
        .arg(parent_dir.join(program_name.as_ref()))
        .output()
        .expect("Failed to run the assembler.");

    if !assemble.status.success() {
        eprintln!("Error running the assembler.");
        std::process::exit(1);
    }

    // clean up when we're done
    match fs::remove_file(parent_dir.join("assembly.s")) {
        Ok(_) => (),
        Err(e) => eprintln!("Failed to delete preprocessed.i: {}", e),
    }


}
