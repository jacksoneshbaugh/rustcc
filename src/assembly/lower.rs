use std::collections::VecDeque;
use crate::assembly::alloc::replace_pseudoregisters;
use crate::assembly::{AssemblyFunction, AssemblyInstruction, AssemblyProgram, Operand, RegisterIdentifier};
use crate::assembly::AssemblyInstruction::{Binary, Cmp, Jmp, JmpCC, Label, Mov, Ret, SetCC, Unary};
use crate::assembly::ConditionalCode::{E, G, GE, L, LE, NE};
use crate::assembly::legalize::polish_program;
use crate::assembly::Operand::{Immediate, PseudoRegister, Register};
use crate::assembly::RegisterIdentifier::AX;
use crate::parser::{BinaryOperator, UnaryOperator};
use crate::tacky::{TACKYFunction, TACKYInstruction, TACKYProgram, TACKYValue, TACKYPlace};

/**
rustcc | assembly/lower.rs
Pass 1/3 in assembly generation; converts TACKY syntax to an assembly AST.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

/// PASS 1 of 3 IN ASSEMBLY GENERATION
/// Converts a TACKY AST into an Assembly AST.
/// Also executes passes 2 and 3.
pub fn generate_assembly_ast(program: TACKYProgram) -> AssemblyProgram {
    let prog = AssemblyProgram {
        function_definition: process_function(
            program.function,
        )
    };

    let results = replace_pseudoregisters(prog);
    let bytes = results.0;
    let intermediate_program = results.1;

    polish_program(intermediate_program, bytes)
}

/// Converts TACKYInstructions into a set of AssemblyInstructions
fn process_instructions(instrs: VecDeque<TACKYInstruction>) -> Vec<AssemblyInstruction> {

    let mut instructions: Vec<AssemblyInstruction> = vec![];

    for instruction in instrs {
        match instruction {
            TACKYInstruction::Return(value) => {
                instructions.push(Mov(process_value(value), Register(RegisterIdentifier::AX)));
                instructions.push(Ret);
            },
            TACKYInstruction::Unary(UnaryOperator::Not, src,dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(Immediate(0), process_value(src)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(E, destination));
            },
            TACKYInstruction::Unary(un_op, src, dest) => {
                let destination = process_place(dest);
                instructions.push(Mov(process_value(src), destination.clone()));
                instructions.push(Unary(un_op, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::GreaterThan, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src2), process_value(src1)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(G, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::GreaterOrEqual, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src2), process_value(src1)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(GE, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::LessThan, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src2), process_value(src1)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(L, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::LessOrEqual, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src2), process_value(src1)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(LE, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::Equal, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src1), process_value(src2)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(E, destination));
            },

            TACKYInstruction::Binary(BinaryOperator::NotEqual, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Cmp(process_value(src1), process_value(src2)));
                instructions.push(Mov(Immediate(0), destination.clone()));
                instructions.push(SetCC(NE, destination));
            },


            TACKYInstruction::Binary(BinaryOperator::Divide, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Mov(process_value(src1), Register(AX)));
                instructions.push(AssemblyInstruction::Cdq);
                let divisor = process_value(src2);
                match divisor {
                    Operand::Immediate(imm) => {
                        instructions.push(Mov(Operand::Immediate(imm), Register(RegisterIdentifier::R10)));
                        instructions.push(AssemblyInstruction::Idiv(Register(RegisterIdentifier::R10)));
                    }
                    _ => {
                        instructions.push(AssemblyInstruction::Idiv(divisor));
                    }
                }
                instructions.push(Mov(Register(AX), destination));
            },

            TACKYInstruction::Binary(BinaryOperator::Remainder, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Mov(process_value(src1), Register(AX)));
                instructions.push(AssemblyInstruction::Cdq);
                let divisor = process_value(src2);
                match divisor {
                    Operand::Immediate(imm) => {
                        instructions.push(Mov(Operand::Immediate(imm), Register(RegisterIdentifier::R10)));
                        instructions.push(AssemblyInstruction::Idiv(Register(RegisterIdentifier::R10)));
                    }
                    _ => {
                        instructions.push(AssemblyInstruction::Idiv(divisor));
                    }
                }
                instructions.push(Mov(Register(RegisterIdentifier::DX), destination));
            },

            TACKYInstruction::Binary(bin_op, src1, src2, dest) => {
                let destination = process_place(dest);
                instructions.push(Mov(process_value(src1), destination.clone()));
                instructions.push(Binary(bin_op, process_value(src2), destination));
            },

            TACKYInstruction::Jump(target) => {
                instructions.push(Jmp(target));
            },

            TACKYInstruction::JumpIfZero(condition, target) => {
                instructions.push(Cmp(Immediate(0), process_value(condition)));
                instructions.push(JmpCC(E, target));
            },

            TACKYInstruction::JumpIfNotZero(condition, target) => {
                instructions.push(Cmp(Immediate(0), process_value(condition)));
                instructions.push(JmpCC(NE, target));
            },

            TACKYInstruction::Copy(src, dest) => {
                instructions.push(Mov(process_value(src), process_place(dest)));
            },

            TACKYInstruction::Label(identifier) => {
                instructions.push(Label(identifier));
            }

        }
    }

    instructions
}

fn process_value(value: TACKYValue) -> Operand {
    match value {
        TACKYValue::Constant(int) => Operand::Immediate(int),
        TACKYValue::Variable(identifier) => PseudoRegister(identifier),
    }
}

fn process_place(place: TACKYPlace) -> Operand {
    Operand::PseudoRegister(place.0)
}

/// Converts a Tacky AST Function node into an Assembly AST Function node
fn process_function(function: TACKYFunction) -> AssemblyFunction {
    AssemblyFunction {
        identifier: function.identifier,
        instructions: VecDeque::from(
            process_instructions(VecDeque::from(function.instructions))
        )
    }
}