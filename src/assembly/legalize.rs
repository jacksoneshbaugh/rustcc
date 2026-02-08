use std::collections::VecDeque;
use crate::assembly::AssemblyInstruction::{AllocateStack, Cmp, Mov};
use crate::assembly::{AssemblyFunction, AssemblyInstruction, AssemblyProgram, Operand, RegisterIdentifier};
use crate::assembly::Operand::{Register, Stack};
use crate::parser::BinaryOperator;

/**
rustcc | assembly/legalize.rs
Pass 3/3 in assembly generation; legalizes invalid assembly syntax.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

/// PASS 3 of 3 IN ASSEMBLY GENERATION
/// Polishes the final pieces of the program. We (1) insert an AllocateStack() instruction
/// and (2) rewrite invalid Mov(), Binary(), and Cmp() instructions.
pub fn polish_program(program: AssemblyProgram, stack_bytes: i32) -> AssemblyProgram {

    let mut new_instructions = VecDeque::new();

    // 1. Insert AllocateStack()
    new_instructions.push_back(AllocateStack(stack_bytes));

    // 2. Fix any invalid Mov() and Binary() instructions
    for instr in program.function_definition.instructions {
        match instr {
            // Shift with memory count: move count to ecx, then use %cl
            AssemblyInstruction::Binary(op @ (BinaryOperator::LeftShift | BinaryOperator::RightShift), Stack(s_src), dest) => {
                new_instructions.push_back(Mov(Stack(s_src), Register(RegisterIdentifier::CX)));
                new_instructions.push_back(AssemblyInstruction::Binary(op, Register(RegisterIdentifier::CX), dest));
            },
            // Shift with non-CX register count: move to ecx, then use %cl
            AssemblyInstruction::Binary(op @ (BinaryOperator::LeftShift | BinaryOperator::RightShift), Register(reg), dest) => {
                match reg {
                    RegisterIdentifier::CX => {
                        new_instructions.push_back(AssemblyInstruction::Binary(op, Register(RegisterIdentifier::CX), dest));
                    }
                    _ => {
                        new_instructions.push_back(Mov(Register(reg), Register(RegisterIdentifier::CX)));
                        new_instructions.push_back(AssemblyInstruction::Binary(op, Register(RegisterIdentifier::CX), dest));
                    }
                }
            },
            Mov(Stack(s1), Stack(s2)) => {
                new_instructions.push_back(Mov(Stack(s1), Register(RegisterIdentifier::R10)));
                new_instructions.push_back(Mov(Register(RegisterIdentifier::R10), Stack(s2)));
            },
            // Use r10d for Add and Subtract memory-to-memory
            AssemblyInstruction::Binary(op @ (BinaryOperator::Add | BinaryOperator::Subtract), Stack(s_src), Stack(s_dst)) => {
                // x86 forbids memory-to-memory arithmetic; use r10d as a temp for add/sub
                new_instructions.push_back(Mov(Stack(s_src), Register(RegisterIdentifier::R10)));
                new_instructions.push_back(AssemblyInstruction::Binary(op, Register(RegisterIdentifier::R10), Stack(s_dst)));
            },
            // Use r10d for bitwise AND/OR/XOR when both operands are memory
            AssemblyInstruction::Binary(op @ (BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseOr | BinaryOperator::Xor), Stack(s_src), Stack(s_dst)) => {
                new_instructions.push_back(Mov(Stack(s_src), Register(RegisterIdentifier::R10)));
                new_instructions.push_back(AssemblyInstruction::Binary(op, Register(RegisterIdentifier::R10), Stack(s_dst)));
            },
            // Use r11d when the destination is memory (any left operand)
            AssemblyInstruction::Binary(BinaryOperator::Multiply, left, Stack(s_dst)) => {
                // Move destination to a temp register, multiply there, then store back
                new_instructions.push_back(Mov(Stack(s_dst), Register(RegisterIdentifier::R11)));
                match left {
                    // GAS two-operand IMUL doesn't accept an immediate; move imm to a register first
                    Operand::Immediate(imm) => {
                        new_instructions.push_back(Mov(Operand::Immediate(imm), Register(RegisterIdentifier::R10)));
                        new_instructions.push_back(AssemblyInstruction::Binary(BinaryOperator::Multiply, Register(RegisterIdentifier::R10), Register(RegisterIdentifier::R11)));
                    }
                    _ => {
                        new_instructions.push_back(AssemblyInstruction::Binary(BinaryOperator::Multiply, left, Register(RegisterIdentifier::R11)));
                    }
                }
                new_instructions.push_back(Mov(Register(RegisterIdentifier::R11), Stack(s_dst)));
            },
            Mov(src, dest) => new_instructions.push_back(Mov(src, dest)),
            Cmp(val1, val2) => {
                match (val1, val2) {
                    // mem vs mem is illegal: move left into r10d
                    (Stack(s1), Stack(s2)) => {
                        new_instructions.push_back(Mov(Stack(s1), Register(RegisterIdentifier::R10)));
                        new_instructions.push_back(Cmp(Register(RegisterIdentifier::R10), Stack(s2)));
                    }

                    // immediate as the *second* operand is illegal: move imm into r10d
                    // (keeps semantics of: cmp val1, val2  ==> flags from (val2 - val1))
                    (left, Operand::Immediate(imm)) => {
                        new_instructions.push_back(Mov(Operand::Immediate(imm), Register(RegisterIdentifier::R10)));
                        new_instructions.push_back(Cmp(left, Register(RegisterIdentifier::R10)));
                    }

                    // otherwise fine
                    (left, right) => {
                        new_instructions.push_back(Cmp(left, right));
                    }
                }
            }
            other => new_instructions.push_back(other)
        }
    }

    AssemblyProgram {
        function_definition: AssemblyFunction {
            identifier: program.function_definition.identifier,
            instructions: new_instructions
        }

    }

}