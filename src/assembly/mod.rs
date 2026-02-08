/**
rustcc | assembly/mod.rs
This module holds the assembly-generating logic, including:
    - Assembly AST
    - Three-Pass Conversion (lower.rs, alloc.rs, legalize.rs)
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

pub mod ast;
pub mod lower;
pub mod alloc;
pub mod legalize;
pub mod emit;

pub use ast::*;
pub use lower::generate_assembly_ast;
pub use emit::emit_assembly;