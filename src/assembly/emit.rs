use std::fs::File;
use std::io::Write;
use crate::assembly::{AssemblyFunction, AssemblyInstruction, AssemblyProgram, ConditionalCode, Operand, RegisterIdentifier};
use crate::assembly::AssemblyInstruction::*;
use crate::assembly::Operand::{Immediate, PseudoRegister, Register, Stack};
use crate::parser::{BinaryOperator, UnaryOperator};

/**
rustcc | assembly/emit.rs
Emits the constructed assembly code.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

/// Generate proper Assembly code for oneself
pub trait AssemblyGeneration {
    /// Generate proper Assembly code from this struct
    fn to_assembly(&self) -> String;
}

impl AssemblyGeneration for UnaryOperator {
    fn to_assembly(&self) -> String {
        match self {
            UnaryOperator::Complement => "notl".into(),
            UnaryOperator::Negate => "negl".into(),
            UnaryOperator::Not => panic!(
                "UnaryOperator::Not is lowered earlier (Cmp + SetCC); should not reach unary asm emission."
            ),
        }
    }
}

pub trait ByteSpecificAssemblyGeneration {
    fn to_assembly(&self, bytes: i32) -> String;
}


impl AssemblyGeneration for AssemblyProgram {
    fn to_assembly(&self) -> String {
        self.function_definition.to_assembly()
    }
}

impl AssemblyGeneration for AssemblyFunction {
    fn to_assembly(&self) -> String {
        let mut output = String::new();
        let tab = "  ";

        output.push_str(format!("{}.globl _{}\n", tab, self.identifier.name).as_str());
        output.push_str(format!("_{}:\n", self.identifier.name).as_str());

        // Emit the prologue
        output.push_str(format!("{}pushq %rbp\n", tab).as_str());
        output.push_str(format!("{}movq %rsp, %rbp\n", tab).as_str());

        for instruction in &self.instructions {
            output.push_str(format!("{}{}\n", tab, instruction.to_assembly()).as_str());
        }

        output
    }
}



impl AssemblyGeneration for AssemblyInstruction {
    fn to_assembly(&self) -> String {
        match self {
            Mov(src, dest) => {
                format!("movl {}, {}", src.to_assembly(4), dest.to_assembly(4))
            },
            Ret => {
                "movq %rbp, %rsp\n  popq %rbp\n  ret".to_string()
            },
            Unary(op, operand) => {
                let un_op = op.to_assembly();
                let oper = operand.to_assembly(4);

                format!("{} {}", un_op, oper)
            },
            AllocateStack(bytes) => {
                format!("subq ${}, %rsp", bytes)
            },
            Binary(op, left, right) => {
                let mnem = match op {
                    BinaryOperator::Add => "addl",
                    BinaryOperator::Subtract => "subl",
                    BinaryOperator::Multiply => "imull",
                    BinaryOperator::BitwiseAnd => "andl",
                    BinaryOperator::BitwiseOr => "orl",
                    BinaryOperator::Xor => "xorl",
                    BinaryOperator::LeftShift => "shll",
                    BinaryOperator::RightShift => "sarl",

                    // Unreachables
                    BinaryOperator::Divide | BinaryOperator::Remainder => unreachable!("Divide & remainder handled via Idiv lowering"),
                    BinaryOperator::And | BinaryOperator::Or => unreachable!("Logical operators are broken into other TACKY code"),
                    BinaryOperator::GreaterOrEqual | BinaryOperator::GreaterThan | BinaryOperator::LessThan
                    | BinaryOperator::LessOrEqual | BinaryOperator::Equal | BinaryOperator::NotEqual => unreachable!("Relational operations are done with cmpl")
                };
                let left_str = match (op, left) {
                    (BinaryOperator::LeftShift,  Operand::Register(RegisterIdentifier::CX)) |
                    (BinaryOperator::RightShift, Operand::Register(RegisterIdentifier::CX)) => "%cl".to_string(),
                    _ => left.to_assembly(4),
                };
                format!("{} {}, {}", mnem, left_str, right.to_assembly(4))
            },
            Idiv(operand) => {
                format!("idivl {}", operand.to_assembly(4))
            },
            Cdq => {
                "cdq".to_string()
            },
            Cmp(left, right) => {
                format!("cmpl {}, {}", left.to_assembly(4), right.to_assembly(4))
            },

            Jmp(target) => {
                // Prefix local labels with L (macOS)
                format!("jmp L{}", target.name)
            },

            JmpCC(cond, target) => {
                format!("j{} L{}", cond.suffix(), target.name)
            },

            SetCC(cond, operand) => {
                format!("set{} {}", cond.suffix(), operand.to_assembly(1))
            },

            Label(identifier) => {
                format!("L{}:", identifier.name)
            }
        }
    }
}

impl ByteSpecificAssemblyGeneration for Operand {
    fn to_assembly(&self, bytes: i32) -> String {
        match self {
            Immediate(imm) => {
                format!("${}", imm)
            },
            Register(identifier) => {
                format!("%{}", identifier.to_assembly(bytes))
            },
            Stack(offset) => {
                format!("-{}(%rbp)", offset)
            }
            PseudoRegister(_) => {unreachable!()},

        }
    }
}

impl ByteSpecificAssemblyGeneration for RegisterIdentifier {
    fn to_assembly(&self, bytes: i32) -> String {
        match bytes {
            4 => match self {
                RegisterIdentifier::AX => "eax".to_string(),
                RegisterIdentifier::DX => "edx".to_string(),
                RegisterIdentifier::CX => "ecx".to_string(),
                RegisterIdentifier::R10 => "r10d".to_string(),
                RegisterIdentifier::R11 => "r11d".to_string(),
            },

            1 => match self {
                RegisterIdentifier::AX => "al".to_string(),
                RegisterIdentifier::DX => "dl".to_string(),
                RegisterIdentifier::CX => "cl".to_string(),
                RegisterIdentifier::R10 => "r10b".to_string(),
                RegisterIdentifier::R11 => "r11b".to_string()
            },

            _ => unreachable!("Only 4- and 1-byte names are supported at this time.")
        }
    }
}

impl ConditionalCode {
    pub fn suffix(&self) -> &'static str {
        match self {
            ConditionalCode::E  => "e",
            ConditionalCode::NE => "ne",
            ConditionalCode::G  => "g",
            ConditionalCode::GE => "ge",
            ConditionalCode::L  => "l",
            ConditionalCode::LE => "le",
        }
    }
}

/// Uses the GenerateAssembly trait that all Assembly AST nodes have to recursively write the program to a file.
pub fn emit_assembly(program: AssemblyProgram, output_file_name: String) {

    let mut file = File::create(output_file_name).unwrap();
    file.write_all(program.to_assembly().as_bytes()).unwrap();

}