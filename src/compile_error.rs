/**
rustcc | compile_error.rs
Holds details related to the errors to possibly raise during compilation.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
    Syntax(String),
    Semantic(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::Io(e) => write!(f, "IO error: {}", e),
            CompileError::Syntax(msg) => write!(f, "Syntax error: {}", msg),
            CompileError::Semantic(msg) => write!(f, "Semantic error: {}", msg),
        }
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}