use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use crate::assembly::AssemblyInstruction::*;
use crate::assembly::Operand::{PseudoRegister, Register, Stack};
use crate::parser::{BinaryOperator, Identifier, PrettyPrint, UnaryOperator};

/**
rustcc | assembly/ast.rs
Holds Abstract Syntax Tree (AST) structures for assembly generation
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
 */

/// Represents a program in the Assembly AST
pub struct AssemblyProgram {
    pub function_definition: AssemblyFunction
}

impl fmt::Display for AssemblyProgram {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl PrettyPrint for AssemblyProgram {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Program(", indent_str)?;
        self.function_definition.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
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
    Unary(UnaryOperator, Operand),
    AllocateStack(i32), // subq $n, %rsp
    Ret,
    Binary(BinaryOperator, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC(ConditionalCode, Identifier),
    SetCC(ConditionalCode, Operand),
    Label(Identifier),
}

impl PrettyPrint for AssemblyInstruction {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            Mov(op1, op2) => {
                writeln!(f, "{}Mov(", indent_str)?;
                op1.pretty_print(f, indent + 1)?;
                op2.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Unary(op, operand) => {
                writeln!(f, "{}Unary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                operand.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            AllocateStack(stack) => writeln!(f, "{}AllocateStack({})", indent_str, stack),
            Ret => writeln!(f, "{}Ret", indent_str),
            Binary(op, left, right) => {
                writeln!(f, "{}Binary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                left.pretty_print(f, indent + 1)?;
                right.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Idiv(op) => {
                writeln!(f, "{}Idiv(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Cdq => writeln!(f, "{}Cdq()", indent_str),
            Cmp(left, right) => {
                writeln!(f, "{}Cmp(", indent_str)?;
                left.pretty_print(f, indent + 1)?;
                right.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Jmp(target) => writeln!(f, "{}Jmp({})", indent_str, target.name),
            JmpCC(cond, target) => writeln!(f, "{}JmpCC({:?}, {})", indent_str, cond, target.name),
            SetCC(cond, operand) => {
                writeln!(f, "{}SetCC(", indent_str)?;
                writeln!(f, "{}  Cond({:?}),", indent_str, cond)?;
                operand.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Label(id) => writeln!(f, "{}Label({})", indent_str, id.name),
        }
    }
}

/// Represents an operand in an instruction in the Assembly AST.
#[derive(Clone)]
pub enum Operand {
    Immediate(i32),
    Register(RegisterIdentifier),
    PseudoRegister(Identifier),
    Stack(i32) // location on the stack (i.e., -i32(%rbp))
}

impl PrettyPrint for Operand {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            Operand::Immediate(imm) => {
                writeln!(f, "{}Immediate({})", indent_str, imm)
            },
            Register(identifier) => {
                write!(f, "{}Register(", indent_str)?;
                identifier.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            PseudoRegister(identifier) => {
                write!(f, "{}PseudoRegister(", indent_str)?;
                identifier.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            Stack(stack) => {
                write!(f, "{}Stack({})", indent_str, stack)
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ConditionalCode {
    E, NE, G, GE, L, LE
}

#[derive(Clone)]
pub enum RegisterIdentifier {
    AX,
    DX,
    CX,
    R10,
    R11
}

impl PrettyPrint for RegisterIdentifier {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        match self {
            RegisterIdentifier::AX => write!(f, "AX"),
            RegisterIdentifier::DX => write!(f, "DX"),
            RegisterIdentifier::CX => write!(f, "CX"),
            RegisterIdentifier::R10 => write!(f, "R10"),
            RegisterIdentifier::R11 => write!(f, "R11"),
        }
    }
}