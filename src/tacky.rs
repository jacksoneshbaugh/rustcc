/*
rustcc | tacky.rs
Contains logic for generating TACKY Three Address Code.
Jackson Eshbaugh
Written while following "Writing a C Compiler" by Nora Sandler
 */
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use crate::compile_error::CompileError;
use crate::parser::{Expression, Function, Identifier, PrettyPrint, Program, Statement, UnaryOperator};
use crate::tacky::TACKYValue::Variable;

pub struct TACKYProgram {
    function: TACKYFunction
}

impl fmt::Display for TACKYProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl PrettyPrint for TACKYProgram {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> Result<(), std::fmt::Error> {

        let indent_str = "  ".repeat(indent);

        writeln!(f, "Program(")?;
        self.function.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}


pub struct TACKYFunction {
    identifier: Identifier,
    instructions: VecDeque<TACKYInstruction>
}

impl PrettyPrint for TACKYFunction {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> Result<(), std::fmt::Error> {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Function({},", indent_str, self.identifier.name)?;
        for instruction in &self.instructions {
            instruction.pretty_print(f, indent + 1)?;
        }
        writeln!(f, "{}", indent_str)
    }
}

pub enum TACKYInstruction {
    Return(TACKYValue),
    //                       src        dest
    Unary(UnaryOperator, TACKYValue, TACKYValue)
}

impl PrettyPrint for TACKYInstruction {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> Result<(), std::fmt::Error> {
        let indent_str = "  ".repeat(indent);

        match self {
            TACKYInstruction::Return(val) => {
                writeln!(f, "{}Return(", indent_str)?;
                val.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            TACKYInstruction::Unary(op, src, dest) => {
                write!(f, "{}Unary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                src.pretty_print(f, indent + 1)?;
                dest.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)

            }
        }
    }
}

#[derive(Clone)]
pub enum TACKYValue {
    Constant(i32),
    Variable(Identifier),
}

impl PrettyPrint for TACKYValue {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            TACKYValue::Constant(val) => {
                writeln!(f, "{}Constant({})", indent_str, val)
            },
            Variable(name) => {
                writeln!(f, "{}Variable({})", indent_str, name.name)
            }
        }
    }
}


pub struct TempAllocator {
    counter: i32,
}

impl TempAllocator {
    pub fn new() -> Self {
        TempAllocator { counter: 0 }
    }

    pub fn allocate(&mut self) -> String {
        let name = self.counter.to_string();
        self.counter += 1;
        name
    }
}


pub fn tackify(ast: Program) -> Result<TACKYProgram, CompileError> {
    let mut allocator = TempAllocator::new();
    Ok(TACKYProgram {
      function: tackify_function(ast.function_definition, &mut allocator)?,
    })
}

fn tackify_function(parsed_fn: Function, allocator: &mut TempAllocator) -> Result<TACKYFunction, CompileError> {
    Ok(TACKYFunction{
        identifier: parsed_fn.identifier,
        instructions: VecDeque::from(tackify_statement(parsed_fn.body, allocator)?)
    })
}

fn tackify_statement(parsed_stmt: Statement, allocator: &mut TempAllocator) -> Result<Vec<TACKYInstruction>, CompileError> {
    match parsed_stmt {
        Statement::Return(exp) => {
            let result = tackify_expression(exp, allocator)?;
            let mut instrs = result.0;

            instrs.push(TACKYInstruction::Return(result.1));
            Ok(instrs)
        }
    }
}

fn tackify_expression(parsed_expr: Expression, allocator: &mut TempAllocator) -> Result<(Vec<TACKYInstruction>, TACKYValue), CompileError> {
    match parsed_expr {
        Expression::Constant(i) => {
            Ok((vec![], TACKYValue::Constant(i)))
        },
        Expression::Unary(op, inner) => unsafe {
            let result = tackify_expression(*inner, allocator)?;
            let mut instrs = result.0;
            let val = result.1;

            let dest_name = allocator.allocate();

            let dest = Variable(Identifier{name: dest_name});

            instrs.push(TACKYInstruction::Unary(op, val, dest.clone()));
            Ok((instrs, dest))
        }
    }
}

