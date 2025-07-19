use crate::assembly::AssemblyInstruction::{AllocateStack, Mov, Ret, Unary};
use crate::assembly::Operand::{PseudoRegister, Register, Stack};
use crate::parser::{Identifier, PrettyPrint, UnaryOperator};
use crate::tacky::{TACKYFunction, TACKYInstruction, TACKYProgram, TACKYValue};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::fmt::Formatter;
use std::fs::File;
use std::io::Write;

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

        // Emit the prologue
        output.push_str(format!("{}pushq %rbp\n", tab).as_str());
        output.push_str(format!("{}movq %rsp, %rbp\n", tab).as_str());

        for instruction in &self.instructions {
            output.push_str(format!("{}{}\n", tab, instruction.to_assembly()).as_str());
        }

        output
    }
}

/// Represents a single instruction in the Assembly AST
pub enum AssemblyInstruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(i32), // subq $n, %rsp
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
            AssemblyInstruction::Unary(op, operand) => {
                writeln!(f, "{}Unary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                operand.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            AssemblyInstruction::AllocateStack(stack) => {
                writeln!(f, "{}AllocateStack({})", indent_str, stack)
            }
            Ret => {
                writeln!(f, "{}Ret", indent_str)
            }
        }
    }
}

impl AssemblyGeneration for AssemblyInstruction {
    fn to_assembly(&self) -> String {
        match self {
            Mov(src, dest) => {
                format!("movl {}, {}", src.to_assembly(), dest.to_assembly())
            },
            Ret => {
                "movq %rbp, %rsp\n  popq %rbp\n  ret".to_string()
            },
            Unary(op, operand) => {
                let un_op = op.to_assembly();
                let oper = operand.to_assembly();

                format!("{} {}", un_op, oper)
            },
            AllocateStack(bytes) => {
                format!("subq ${}, %rsp", bytes)
            }
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

impl AssemblyGeneration for Operand {
    fn to_assembly(&self) -> String {
        match self {
            Operand::Immediate(imm) => {
                format!("${}", imm)
            },
            Register(identifier) => {
                format!("%{}", identifier.to_assembly())
            },
            Stack(offset) => {
                format!("-{}(%rbp)", offset)
            }
            PseudoRegister(_) => {unreachable!()},

        }
    }
}

#[derive(Clone)]
pub enum RegisterIdentifier {
    AX,
    R10
}

impl PrettyPrint for RegisterIdentifier {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        match self {
            RegisterIdentifier::AX => write!(f, "AX"),
            RegisterIdentifier::R10 => write!(f, "R10")
        }
    }
}

impl AssemblyGeneration for RegisterIdentifier {
    fn to_assembly(&self) -> String {
        match self {
            RegisterIdentifier::AX => "eax".to_string(),
            RegisterIdentifier::R10 => "r10d".to_string()
        }
    }
}

/// Uses the GenerateAssembly trait that all Assembly AST nodes have to recursively write the program to a file.
pub fn emit_assembly(program: AssemblyProgram, output_file_name: String) {

    let mut file = File::create(output_file_name).unwrap();
    file.write_all(program.to_assembly().as_bytes()).unwrap();

}


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


/// PASS 3 of 3 IN ASSEMBLY GENERATION
/// Polishes the final pieces of the program. We (1) insert an AllocateStack() instruction
/// and (2) rewrite invalid Mov() instructions.
pub fn polish_program(program: AssemblyProgram, stack_bytes: i32) -> AssemblyProgram {

    let mut new_instructions = VecDeque::new();

    // 1. Insert AllocateStack()
    new_instructions.push_back(AllocateStack(stack_bytes));

    // 2. Fix any invalid Mov() instructions
    for instr in program.function_definition.instructions {
        match instr {
            Mov(Stack(s1), Stack(s2)) => {
                new_instructions.push_back(Mov(Stack(s1), Register(RegisterIdentifier::R10)));
                new_instructions.push_back(Mov(Register(RegisterIdentifier::R10), Stack(s2)));
            },
            Mov(src, dest) => new_instructions.push_back(Mov(src, dest)),
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


/// Converts a C AST Function node into an Assembly AST Function node
fn process_function(function: TACKYFunction) -> AssemblyFunction {
    AssemblyFunction {
        identifier: function.identifier,
        instructions: VecDeque::from(
            process_instructions(function.instructions)
        )
    }
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
            TACKYInstruction::Unary(un_op, src, dest) => {
                let destination = process_value(dest);
                instructions.push(Mov(process_value(src), destination.clone()));
                instructions.push(Unary(un_op, destination));
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