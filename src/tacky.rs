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
use crate::parser::{BinaryOperator, Expression, Function, Identifier, PrettyPrint, Program, Statement, UnaryOperator};
use crate::tacky::TACKYValue::Variable;

pub struct TACKYProgram {
    pub(crate) function: TACKYFunction
}

impl fmt::Display for TACKYProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl PrettyPrint for TACKYProgram {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> Result<(), fmt::Error> {

        let indent_str = "  ".repeat(indent);

        writeln!(f, "Program(")?;
        self.function.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}


pub struct TACKYFunction {
    pub(crate) identifier: Identifier,
    pub(crate) instructions: VecDeque<TACKYInstruction>
}

impl PrettyPrint for TACKYFunction {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> Result<(), fmt::Error> {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Function({},", indent_str, self.identifier.name)?;
        for instruction in &self.instructions {
            instruction.pretty_print(f, indent + 1)?;
        }
        writeln!(f, "{})", indent_str)
    }
}

pub enum TACKYInstruction {
    Return(TACKYValue),
    //                       src        dest
    Unary(UnaryOperator, TACKYValue, TACKYValue),
    //                        src1        src2        dest
    Binary(BinaryOperator, TACKYValue, TACKYValue, TACKYValue),
}

impl PrettyPrint for TACKYInstruction {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> Result<(), fmt::Error> {
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
                writeln!(f, ",")?;
                dest.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            TACKYInstruction::Binary(op, src1, src2, dest) => {
                write!(f, "{}Binary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                src1.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                src2.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
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
        Expression::Unary(op, inner) => {
            let result = tackify_expression(*inner, allocator)?;
            let mut instrs = result.0;
            let val = result.1;

            let dest_name = allocator.allocate();

            let dest = Variable(Identifier{name: dest_name});

            instrs.push(TACKYInstruction::Unary(op, val, dest.clone()));
            Ok((instrs, dest))
        },
        Expression::Binary(op, left, right) => {
            let v1 = tackify_expression(*left, allocator)?;
            let v2 = tackify_expression(*right, allocator)?;

            let dst_name = allocator.allocate();
            let dest = Variable(Identifier{name: dst_name});

            let mut instrs = v1.0;
            instrs.extend(v2.0);
            instrs.push(TACKYInstruction::Binary(op, v1.1, v2.1, dest.clone()));

            Ok((instrs, dest))
        }
    }
}

