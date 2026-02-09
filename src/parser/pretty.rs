use std::fmt;
use std::fmt::Formatter;

use super::ast::*;

pub trait PrettyPrint {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result;
}

impl PrettyPrint for Program {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        writeln!(f, "{}Program(", indent_str)?;
        self.function_definition.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", indent_str)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        writeln!(f, "{}Function(", indent_str)?;
        self.identifier.pretty_print(f, indent + 1)?;

        writeln!(f, "{}Body(", "  ".repeat(indent + 1))?;
        for item in &self.body {
            item.pretty_print(f, indent + 2)?;
        }
        writeln!(f, "{})", "  ".repeat(indent + 1))?;

        writeln!(f, "{})", indent_str)
    }
}

impl PrettyPrint for BlockItem {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            BlockItem::Statement(stmt) => {
                writeln!(f, "{}Statement(", indent_str)?;
                stmt.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            BlockItem::Declaration(decl) => {
                writeln!(f, "{}Declaration(", indent_str)?;
                decl.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
        }
    }
}

impl PrettyPrint for Statement {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            Statement::Return(exp) => {
                writeln!(f, "{}Return(", indent_str)?;
                exp.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Statement::Expression(exp) => {
                writeln!(f, "{}Expression(", indent_str)?;
                exp.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
            Statement::Null => {
                writeln!(f, "{}Null", indent_str)
            }
        }
    }
}

impl PrettyPrint for Declaration {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            Declaration::Declaration(id, init) => {
                writeln!(f, "{}Declaration(", indent_str)?;
                id.pretty_print(f, indent + 1)?;

                match init {
                    Some(expr) => {
                        writeln!(f, "{}Initializer(", "  ".repeat(indent + 1))?;
                        expr.pretty_print(f, indent + 2)?;
                        writeln!(f, "{})", "  ".repeat(indent + 1))?;
                    }
                    None => {
                        writeln!(f, "{}Initializer(None)", "  ".repeat(indent + 1))?;
                    }
                }

                writeln!(f, "{})", indent_str)
            }
        }
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        match self {
            Expression::Constant(val) => writeln!(f, "{}Constant({})", indent_str, val),

            Expression::Variable(id) => writeln!(f, "{}Variable({})", indent_str, id.name),

            Expression::Unary(op, expr) => {
                writeln!(f, "{}Unary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }

            Expression::Binary(op, left, right) => {
                writeln!(f, "{}Binary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                left.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                right.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }

            Expression::Assignment(lhs, rhs) => {
                writeln!(f, "{}Assignment(", indent_str)?;
                lhs.pretty_print(f, indent + 1)?;
                writeln!(f, ",")?;
                rhs.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
        }
    }
}

impl PrettyPrint for UnaryOperator {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        match self {
            UnaryOperator::Complement => write!(f, "Complement"),
            UnaryOperator::Negate => write!(f, "Negate"),
            UnaryOperator::Not => write!(f, "Not"),
        }
    }
}

impl PrettyPrint for BinaryOperator {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        use BinaryOperator::*;
        match self {
            Add => write!(f, "Add"),
            Subtract => write!(f, "Subtract"),
            Multiply => write!(f, "Multiply"),
            Divide => write!(f, "Divide"),
            Remainder => write!(f, "Remainder"),

            BitwiseAnd => write!(f, "BitwiseAnd"),
            BitwiseOr => write!(f, "BitwiseOr"),
            Xor => write!(f, "Xor"),
            LeftShift => write!(f, "LeftShift"),
            RightShift => write!(f, "RightShift"),

            And => write!(f, "And"),
            Or => write!(f, "Or"),

            Equal => write!(f, "Equal"),
            NotEqual => write!(f, "NotEqual"),
            LessThan => write!(f, "LessThan"),
            LessOrEqual => write!(f, "LessOrEqual"),
            GreaterThan => write!(f, "GreaterThan"),
            GreaterOrEqual => write!(f, "GreaterOrEqual"),
        }
    }
}

impl PrettyPrint for Identifier {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        writeln!(f, "Identifier({})", self.name)
    }
}