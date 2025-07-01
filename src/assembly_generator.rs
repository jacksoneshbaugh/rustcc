use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use crate::assembly_generator::AssemblyInstruction::Ret;
use crate::parser::{Expression, Function, Identifier, PrettyPrint, Program, Statement};

/**
rustcc | assembly_generator.rs
Holds details and logic necessary to generating assembly instructions from AST.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/


/// Represents a program in the Assembly AST
pub struct AssemblyProgram {
    pub function_definition: AssemblyFunction
}

impl PrettyPrint for AssemblyProgram {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Program(", indent_str)?;
        self.function_definition.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}

impl fmt::Display for AssemblyProgram {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

/// Represents a function in the Assembly AST
pub struct AssemblyFunction {
    pub identifier: Identifier,
    pub instructions: VecDeque<AssemblyInstruction>
}

impl PrettyPrint for AssemblyFunction {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            AssemblyFunction {identifier, instructions} => {
                writeln!(f, "{}Function(", indent_str)?;
                identifier.pretty_print(f, indent + 1)?;
                for instruction in instructions {
                    instruction.pretty_print(f, indent + 1)?;
                }
                writeln!(f, "{})", indent_str)
            }
        }
    }
}

/// Represents a single instruction in the Assembly AST
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Ret
}

impl PrettyPrint for AssemblyInstruction {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            AssemblyInstruction::Mov(op1, op2) => {
                writeln!(f, "{}Mov(", indent_str)?;
                op1.pretty_print(f, indent + 1)?;
                op2.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            AssemblyInstruction::Ret => {
                writeln!(f, "{}Ret", indent_str)
            }
        }
    }
}

/// Represents an operand in an instruction in the Assembly AST.
pub enum Operand {
    Immediate(i32),
    Register(String),
}

impl PrettyPrint for Operand {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            Operand::Immediate(imm) => {
                writeln!(f, "{}Immediate({})", indent_str, imm)
            },
            Operand::Register(name) => {
                writeln!(f, "{}Register({})", indent_str, name)
            }
        }
    }
}



/// Converts a C AST into an Assembly AST
pub fn generate_assembly_ast(program: Program) -> AssemblyProgram {
    AssemblyProgram {
        function_definition: process_function(
            program.function_definition,
        )
    }
}


/// Converts a C AST Function node into an Assembly AST Function node
fn process_function(function: Function) -> AssemblyFunction {
    AssemblyFunction {
        identifier: function.identifier,
        instructions: VecDeque::from(
            process_statement(function.body)
        )
    }
}


/// Converts a C AST Statement node into an Assembly AST Statement node
fn process_statement(stmt: Statement) -> Vec<AssemblyInstruction> {
    match stmt {
        Statement::Return(exp) => {
            let (mut instructions, operand) = process_expression(exp);
            instructions.push(AssemblyInstruction::Mov(operand, Operand::Register("eax".to_string())));
            instructions.push(Ret);
            instructions
        }
    }
}

/// Processes an expression, returning the set of instructions needed and the operand where the result can be found.
fn process_expression(expr: Expression) -> (Vec<AssemblyInstruction>, Operand) {
    match expr {
        Expression::Constant(val) => (vec![], Operand::Immediate(val)),
    }
}