use std::fmt;
use std::fmt::Formatter;

use super::ast::*;

pub trait PrettyPrint {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result;
}

fn ind(n: usize) -> String {
    "  ".repeat(n)
}

fn comma_line(f: &mut Formatter, indent: usize) -> fmt::Result {
    writeln!(f, "{},", ind(indent))
}

/* ---------------- Program / Function / Block ---------------- */

impl PrettyPrint for Program {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{}Program(", ind(indent))?;
        self.function_definition.pretty_print(f, indent + 1)?;
        writeln!(f, "{})", ind(indent))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{}Function(", ind(indent))?;

        self.identifier.pretty_print(f, indent + 1)?;
        comma_line(f, indent + 1)?;

        writeln!(f, "{}Body(", ind(indent + 1))?;
        self.body.pretty_print(f, indent + 2)?;
        writeln!(f, "{})", ind(indent + 1))?;

        writeln!(f, "{})", ind(indent))
    }
}

impl PrettyPrint for Block {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{}Block(", ind(indent))?;
        for (i, item) in self.items.iter().enumerate() {
            item.pretty_print(f, indent + 1)?;
            if i + 1 != self.items.len() {
                comma_line(f, indent + 1)?;
            }
        }
        writeln!(f, "{})", ind(indent))
    }
}

/* ---------------- BlockItem ---------------- */

impl PrettyPrint for BlockItem {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        match self {
            BlockItem::Statement(stmt) => {
                writeln!(f, "{}Statement(", ind(indent))?;
                stmt.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
            BlockItem::Declaration(decl) => {
                writeln!(f, "{}DeclarationItem(", ind(indent))?;
                decl.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
        }
    }
}

/* ---------------- Statements ---------------- */

impl PrettyPrint for Statement {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        match self {
            Statement::Return(exp) => {
                writeln!(f, "{}Return(", ind(indent))?;
                exp.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Statement::Expression(exp) => {
                writeln!(f, "{}ExpressionStmt(", ind(indent))?;
                exp.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Statement::Compound(block) => {
                writeln!(f, "{}Compound(", ind(indent))?;
                block.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Statement::Goto(label) => {
                writeln!(f, "{}Goto(", ind(indent))?;
                label.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Statement::Label(name, stmt) => {
                writeln!(f, "{}Label(", ind(indent))?;
                name.pretty_print(f, indent + 1)?;
                comma_line(f, indent + 1)?;
                writeln!(f, "{}Body(", ind(indent + 1))?;
                stmt.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;
                writeln!(f, "{})", ind(indent))
            }

            Statement::Null => writeln!(f, "{}Null", ind(indent)),

            Statement::If(cond, then_s, else_s) => {
                writeln!(f, "{}If(", ind(indent))?;

                writeln!(f, "{}Condition(", ind(indent + 1))?;
                cond.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;
                comma_line(f, indent + 1)?;

                writeln!(f, "{}Then(", ind(indent + 1))?;
                then_s.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;

                if let Some(el) = else_s.as_deref() {
                    comma_line(f, indent + 1)?;
                    writeln!(f, "{}Else(", ind(indent + 1))?;
                    el.pretty_print(f, indent + 2)?;
                    writeln!(f, "{})", ind(indent + 1))?;
                }

                writeln!(f, "{})", ind(indent))
            }
        }
    }
}

/* ---------------- Declarations ---------------- */

impl PrettyPrint for Declaration {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        match self {
            Declaration::Declaration(id, init) => {
                writeln!(f, "{}Declaration(", ind(indent))?;
                id.pretty_print(f, indent + 1)?;

                match init {
                    Some(expr) => {
                        comma_line(f, indent + 1)?;
                        writeln!(f, "{}Initializer(", ind(indent + 1))?;
                        expr.pretty_print(f, indent + 2)?;
                        writeln!(f, "{})", ind(indent + 1))?;
                    }
                    None => {
                        comma_line(f, indent + 1)?;
                        writeln!(f, "{}Initializer(None)", ind(indent + 1))?;
                    }
                }

                writeln!(f, "{})", ind(indent))
            }
        }
    }
}

/* ---------------- Expressions ---------------- */

impl PrettyPrint for Expression {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        match self {
            Expression::Constant(val) => writeln!(f, "{}Constant({})", ind(indent), val),

            Expression::Variable(id) => writeln!(f, "{}Variable({})", ind(indent), id.name),

            Expression::Unary(op, expr) => {
                writeln!(f, "{}Unary(", ind(indent))?;
                write!(f, "{}", ind(indent + 1))?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f)?;
                comma_line(f, indent + 1)?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Expression::Binary(op, left, right) => {
                writeln!(f, "{}Binary(", ind(indent))?;
                write!(f, "{}", ind(indent + 1))?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f)?;
                comma_line(f, indent + 1)?;
                left.pretty_print(f, indent + 1)?;
                comma_line(f, indent + 1)?;
                right.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Expression::Ternary(cond, then_e, else_e) => {
                writeln!(f, "{}Ternary(", ind(indent))?;

                writeln!(f, "{}Condition(", ind(indent + 1))?;
                cond.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;
                comma_line(f, indent + 1)?;

                writeln!(f, "{}Then(", ind(indent + 1))?;
                then_e.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;
                comma_line(f, indent + 1)?;

                writeln!(f, "{}Else(", ind(indent + 1))?;
                else_e.pretty_print(f, indent + 2)?;
                writeln!(f, "{})", ind(indent + 1))?;

                writeln!(f, "{})", ind(indent))
            }

            Expression::Assignment(assign_op, lhs, rhs) => {
                writeln!(f, "{}Assignment(", ind(indent))?;

                write!(f, "{}", ind(indent + 1))?;
                assign_op.pretty_print(f, indent + 1)?;
                writeln!(f)?;
                comma_line(f, indent + 1)?;

                lhs.pretty_print(f, indent + 1)?;
                comma_line(f, indent + 1)?;

                rhs.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }

            Expression::PreInc(expr) => {
                writeln!(f, "{}PreInc(", ind(indent))?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
            Expression::PreDec(expr) => {
                writeln!(f, "{}PreDec(", ind(indent))?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
            Expression::PostInc(expr) => {
                writeln!(f, "{}PostInc(", ind(indent))?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
            Expression::PostDec(expr) => {
                writeln!(f, "{}PostDec(", ind(indent))?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", ind(indent))
            }
        }
    }
}

/* ---------------- Operators ---------------- */

impl PrettyPrint for AssignOp {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        let s = match self {
            AssignOp::Assign => "Assign(=)",
            AssignOp::AddAssign => "AddAssign(+=)",
            AssignOp::SubAssign => "SubAssign(-=)",
            AssignOp::MulAssign => "MulAssign(*=)",
            AssignOp::DivAssign => "DivAssign(/=)",
            AssignOp::ModAssign => "ModAssign(%=)",
            AssignOp::AndAssign => "AndAssign(&=)",
            AssignOp::OrAssign => "OrAssign(|=)",
            AssignOp::XorAssign => "XorAssign(^=)",
            AssignOp::ShlAssign => "ShlAssign(<<=)",
            AssignOp::ShrAssign => "ShrAssign(>>=)",
        };
        write!(f, "{}", s)
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

/* ---------------- Identifier ---------------- */

impl PrettyPrint for Identifier {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{}Identifier({})", ind(indent), self.name)
    }
}