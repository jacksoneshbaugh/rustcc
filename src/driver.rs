/**
rustcc | src/driver.rs
Compiler driver: runs the compilation pipeline
(preprocess → lex → parse → tacky → asm ast → emit asm → assemble/link).
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/
use std::collections::VecDeque;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::{assembly, lexer, parser, tacky};

#[derive(Debug, Clone, Copy, Default)]
pub struct DriverFlags {
    pub lex: bool,            // --lex
    pub parse: bool,          // --parse
    pub tacky: bool,          // --tacky
    pub codegen: bool,        // --codegen (dump asm AST)
    pub emit_asm_only: bool,  // -S (stop after writing .s)
    pub keep_temps: bool,     // optional: keep .i/.s files
    pub verbose: bool,        // optional: extra prints
}

#[derive(Debug)]
pub enum DriverStop {
    Done,                // fully compiled and linked
    StoppedAfterLex,
    StoppedAfterParse,
    StoppedAfterTacky,
    StoppedAfterCodegen,
    StoppedAfterEmitAsm, // written .s, stopped due to -S
}

/// A lightweight compilation context (paths derived from input).
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct CompilePaths {
    pub input_c: PathBuf,
    pub parent_dir: PathBuf,
    pub stem: String,
    pub preprocessed_i: PathBuf,
    pub assembly_s: PathBuf,
    pub output_bin: PathBuf,
}

impl CompilePaths {
    pub fn from_input(input: &Path) -> Result<Self, String> {
        if !input.exists() {
            return Err(format!("Input file does not exist: {}", input.display()));
        }

        let parent_dir = input
            .parent()
            .ok_or_else(|| format!("Could not find parent directory for {}", input.display()))?
            .to_path_buf();

        let stem = input
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| format!("Could not extract UTF-8 file stem from {}", input.display()))?
            .to_string();

        let preprocessed_i = parent_dir.join(format!("{stem}.i"));
        let assembly_s = parent_dir.join(format!("{stem}.s"));
        let output_bin = parent_dir.join(&stem);

        Ok(Self {
            input_c: input.to_path_buf(),
            parent_dir,
            stem,
            preprocessed_i,
            assembly_s,
            output_bin,
        })
    }
}

/// Public entrypoint for the driver. Returns where it stopped.
pub fn compile_file(input: &Path, flags: DriverFlags) -> Result<(DriverStop, CompilePaths), String> {
    let paths = CompilePaths::from_input(input)?;

    preprocess(&paths)?;

    // ---- LEX ----
    let tokens = lexer::lex(
        paths
            .preprocessed_i
            .to_str()
            .ok_or_else(|| "Preprocessed path not valid UTF-8".to_string())?,
    )
        .map_err(|e| format!("Lexing failed.\n{e}"))?;

    if flags.verbose {
        eprintln!("Lexing completed successfully.");
    }

    if flags.lex {
        for t in &tokens {
            println!("{}", t.kind);
        }
        cleanup(&paths, flags.keep_temps)?;
        return Ok((DriverStop::StoppedAfterLex, paths));
    }

    // ---- PARSE ----
    let mut tokens_deque = VecDeque::from(tokens);
    let program = parser::parse(&mut tokens_deque)
        .map_err(|e| format!("Parsing failed.\n{e}"))?;

    if flags.verbose {
        eprintln!("Parsing completed successfully.");
    }

    if flags.parse {
        println!("{program}");
        cleanup(&paths, flags.keep_temps)?;
        return Ok((DriverStop::StoppedAfterParse, paths));
    }

    // ---- TACKY ----
    let tacky_prog = tacky::tackify(program)
        .map_err(|e| format!("TACKY lowering failed.\n{e}"))?;

    if flags.verbose {
        eprintln!("TACKY lowering completed successfully.");
    }

    if flags.tacky {
        println!("{tacky_prog}");
        cleanup(&paths, flags.keep_temps)?;
        return Ok((DriverStop::StoppedAfterTacky, paths));
    }

    // ---- ASM AST ----
    let assembly_ast = assembly::generate_assembly_ast(tacky_prog);

    if flags.verbose {
        eprintln!("Assembly AST generation completed successfully.");
    }

    if flags.codegen {
        println!("{assembly_ast}");
        cleanup(&paths, flags.keep_temps)?;
        return Ok((DriverStop::StoppedAfterCodegen, paths));
    }

    // ---- EMIT .s ----
    assembly::emit_assembly(
        assembly_ast,
        paths
            .assembly_s
            .to_str()
            .ok_or_else(|| "Assembly path not valid UTF-8".to_string())?
            .to_string(),
    );

    // Remove preprocessed .i once we have .s
    if !flags.keep_temps {
        let _ = fs::remove_file(&paths.preprocessed_i);
    }

    if flags.emit_asm_only {
        return Ok((DriverStop::StoppedAfterEmitAsm, paths));
    }

    // ---- ASSEMBLE + LINK ----
    assemble_and_link(&paths)?;

    if !flags.keep_temps {
        let _ = fs::remove_file(&paths.assembly_s);
    }

    Ok((DriverStop::Done, paths))
}

fn preprocess(paths: &CompilePaths) -> Result<(), String> {
    let out = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&paths.input_c)
        .arg("-o")
        .arg(&paths.preprocessed_i)
        .output()
        .map_err(|e| format!("Failed to run the preprocessor: {e}"))?;

    if !out.status.success() {
        return Err(format!(
            "Error running the preprocessor.\n{}",
            String::from_utf8_lossy(&out.stderr)
        ));
    }

    Ok(())
}

fn assemble_and_link(paths: &CompilePaths) -> Result<(), String> {
    let out = Command::new("gcc")
        .arg(&paths.assembly_s)
        .arg("-o")
        .arg(&paths.output_bin)
        .arg("-arch")
        .arg("x86_64")
        .output()
        .map_err(|e| format!("Failed to run the assembler/linker: {e}"))?;

    if !out.status.success() {
        return Err(format!(
            "Error running the assembler/linker.\n{}",
            String::from_utf8_lossy(&out.stderr)
        ));
    }

    Ok(())
}

fn cleanup(paths: &CompilePaths, keep: bool) -> Result<(), String> {
    if keep {
        return Ok(());
    }

    // Best-effort cleanup; don’t fail compilation due to cleanup errors.
    let _ = fs::remove_file(&paths.preprocessed_i);
    Ok(())
}