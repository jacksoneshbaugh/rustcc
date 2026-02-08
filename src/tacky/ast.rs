use std::fmt;
use std::fmt::Formatter;
use crate::parser::{BinaryOperator, Identifier, PrettyPrint, UnaryOperator};

/**
rustcc | tacky/ast.rs
Defines structures used to construct the TACKY AST.
Jackson Eshbaugh
Written while following "Writing a C Compiler" by Nora Sandler
*/

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
    pub(crate) instructions: Vec<TACKYInstruction>
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

/// Restrict the destinations to be locations (and not immediates)
#[derive(Clone)]
pub struct TACKYPlace(pub Identifier);

impl PrettyPrint for TACKYPlace {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> Result<(), fmt::Error> {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Place(", indent_str)?;
        self.0.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}
pub enum TACKYInstruction {
    Return(TACKYValue),
    //                       src        dest
    Unary(UnaryOperator, TACKYValue, TACKYPlace),
    //                        src1        src2        dest
    Binary(BinaryOperator, TACKYValue, TACKYValue, TACKYPlace),
    //       src        dest
    Copy(TACKYValue, TACKYPlace),
    //     target
    Jump(Identifier),
    //         condition     target
    JumpIfZero(TACKYValue, Identifier),
    //             condition    target
    JumpIfNotZero(TACKYValue, Identifier),

    Label(Identifier),
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
            }
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

            TACKYInstruction::Copy(src, dest) => {
                writeln!(f, "{}Copy(", indent_str)?;
                src.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                dest.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }

            TACKYInstruction::Jump(target) => {
                writeln!(f, "{}Jump({})", indent_str, target.name)
            }

            TACKYInstruction::JumpIfZero(cond, target) => {
                writeln!(f, "{}JumpIfZero(", indent_str)?;
                cond.pretty_print(f, indent + 1)?;
                writeln!(f, "{},", indent_str)?;
                writeln!(f, "{}  Target({})", indent_str, target.name)?;
                writeln!(f, "{})", indent_str)
            }

            TACKYInstruction::JumpIfNotZero(cond, target) => {
                writeln!(f, "{}JumpIfNotZero(", indent_str)?;
                cond.pretty_print(f, indent + 1)?;
                writeln!(f, "{},", indent_str)?;
                writeln!(f, "{}  Target({})", indent_str, target.name)?;
                writeln!(f, "{})", indent_str)
            }

            TACKYInstruction::Label(id) => {
                writeln!(f, "{}Label({})", indent_str, id.name)
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
            TACKYValue::Variable(name) => {
                writeln!(f, "{}Variable({})", indent_str, name.name)
            }
        }
    }
}