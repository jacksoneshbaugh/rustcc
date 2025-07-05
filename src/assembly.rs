use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use std::fs::File;
use std::io::Write;
use crate::assembly::AssemblyInstruction::Ret;
use crate::parser::{Expression, Function, Identifier, PrettyPrint, Program, Statement};

/**
rustcc | assembly.rs
Holds details and logic necessary to generating assembly instructions from AST and
saving them.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/


/// Represents a program in the Assembly AST
pub struct AssemblyProgram {
    pub function_definition: AssemblyFunction
}

/// Generate proper Assembly code for oneself
pub trait AssemblyGeneration {
    /// Generate proper Assembly code from this struct
    fn to_assembly(&self) -> String;
}

impl PrettyPrint for AssemblyProgram {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Program(", indent_str)?;
        self.function_definition.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}

impl AssemblyGeneration for AssemblyProgram {
    fn to_assembly(&self) -> String {
        self.function_definition.to_assembly()
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

impl AssemblyGeneration for AssemblyFunction {
    fn to_assembly(&self) -> String {
        let mut output = String::new();
        let tab = "  ";

        output.push_str(format!("{}.globl _{}\n", tab, self.identifier.name).as_str());
        output.push_str(format!("_{}:\n", self.identifier.name).as_str());

        for instruction in &self.instructions {
            output.push_str(format!("{}{}\n", tab, instruction.to_assembly()).as_str());
        }

        output
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
            Ret => {
                writeln!(f, "{}Ret", indent_str)
            }
        }
    }
}

impl AssemblyGeneration for AssemblyInstruction {
    fn to_assembly(&self) -> String {
        match self {
            AssemblyInstruction::Mov(src, dest) => {
                format!("mov {}, {}", src.to_assembly(), dest.to_assembly())
            },
            Ret => {
                "ret".to_string()
            }
        }
    }
}

/// Represents an operand in an instruction in the Assembly AST.
pub enum Operand {
    Immediate(i32),
    Register(RegisterIdentifier),
}

impl PrettyPrint for Operand {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            Operand::Immediate(imm) => {
                writeln!(f, "{}Immediate({})", indent_str, imm)
            },
            Operand::Register(identifier) => {
                write!(f, "{}Register(", indent_str)?;
                identifier.pretty_print(f, indent + 1)?;
                writeln!(f, ")")
            }
        }
    }
}

impl AssemblyGeneration for Operand {
    fn to_assembly(&self) -> String {
        match self {
            Operand::Immediate(imm) => {
                format!("${}", imm)
            },
            Operand::Register(identifier) => {
                format!("%{}", identifier.to_assembly())
            }
        }
    }
}

pub enum RegisterIdentifier {
    EAX
}

impl PrettyPrint for RegisterIdentifier {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        match self {
            RegisterIdentifier::EAX => write!(f, "EAX"),
        }
    }
}

impl AssemblyGeneration for RegisterIdentifier {
    fn to_assembly(&self) -> String {
        match self {
            RegisterIdentifier::EAX => "eax".to_string()
        }
    }
}

/// Uses the GenerateAssembly trait that all Assembly AST nodes have to recursively write the program to a file.
pub fn emit_assembly(program: AssemblyProgram, output_file_name: String) {

    let mut file = File::create(output_file_name).unwrap();
    file.write_all(program.to_assembly().as_bytes()).unwrap();

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
            instructions.push(AssemblyInstruction::Mov(operand, Operand::Register(RegisterIdentifier::EAX)));
            instructions.push(Ret);
            instructions
        }
    }
}

/// Processes an expression, returning the set of instructions needed and the operand where the result can be found.
fn process_expression(expr: Expression) -> (Vec<AssemblyInstruction>, Operand) {
    match expr {
        Expression::Constant(val) => (vec![], Operand::Immediate(val)),
        Expression::Unary(op, operand) => {
            (vec![], Operand::Immediate(2)) // TODO: Remove
        }
    }
}