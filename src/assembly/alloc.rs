use std::collections::{HashMap, VecDeque};
use crate::assembly::AssemblyInstruction::{Cmp, Mov, SetCC, Unary};
use crate::assembly::{AssemblyFunction, AssemblyInstruction, AssemblyProgram, Operand};
use crate::assembly::Operand::{PseudoRegister, Stack};

/**
rustcc | assembly/alloc.rs
Pass 2/3 in assembly generation; allocates locations on the stack for pesudoregisters.
Jackson Eshbaugh
Written while following the book "Write a C Compiler" by Nora Sandler.
*/

/// PASS 2 of 3 IN ASSEMBLY GENERATION
/// Replaces all pseudoregisters with Stack() positions.
/// Returns the number of bytes to allocate on the stack and the updated AST.
pub fn replace_pseudoregisters(program: AssemblyProgram) -> (i32, AssemblyProgram) {
    let mut map = HashMap::new();
    let mut num_vars = 0;

    let mut new_instructions = VecDeque::new();

    for instr in program.function_definition.instructions {
        let transformed = match instr {
            Mov(src, dest) => {
                let new_src = replace_operand(src, &mut map, &mut num_vars);
                let new_dest = replace_operand(dest, &mut map, &mut num_vars);
                Mov(new_src, new_dest)
            },
            Unary(op, operand) => {
                let new_operand = replace_operand(operand, &mut map, &mut num_vars);
                Unary(op, new_operand)
            },
            AssemblyInstruction::Binary(op, left, right) => {
                let new_left = replace_operand(left, &mut map, &mut num_vars);
                let new_right = replace_operand(right, &mut map, &mut num_vars);
                AssemblyInstruction::Binary(op, new_left, new_right)
            },
            AssemblyInstruction::Idiv(operand) => {
                let new_operand = replace_operand(operand, &mut map, &mut num_vars);
                AssemblyInstruction::Idiv(new_operand)
            },
            Cmp(val1, val2) => {
                let new_val1 = replace_operand(val1, &mut map, &mut num_vars);
                let new_val2 = replace_operand(val2, &mut map, &mut num_vars);
                Cmp(new_val1, new_val2)
            },
            SetCC(cond, operand) => {
                let new_operand = replace_operand(operand, &mut map, &mut num_vars);
                SetCC(cond, new_operand)
            }
            other => other,
        };
        new_instructions.push_back(transformed);
    }

    let updated_program = AssemblyProgram {
        function_definition: AssemblyFunction {
            identifier: program.function_definition.identifier,
            instructions: new_instructions,
        }
    };

    (num_vars * 4, updated_program)
}

fn replace_operand(operand: Operand, map: &mut HashMap<String, Operand>, num_vars: &mut i32) -> Operand {
    match operand {
        PseudoRegister(identifier) => {
            let name = identifier.name;
            map.entry(name.clone()).or_insert_with(|| {
                *num_vars += 1;
                Stack(*num_vars * 4)
            }).clone()
        }
        other => other,
    }
}