use crate::compile_error::CompileError;
use crate::lexer::{Token, TokenKind};
use crate::parser::Expression::{Binary, Constant, Unary};
use crate::parser::Statement::Return;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use crate::parser::UnaryOperator::{Complement, Negate, Not};
use crate::lexer::TokenKind::{CloseBrace, CloseParen, IdentifierToken, Int, OpenBrace, OpenParen, Semicolon, Void};

/**
rustcc | parser.rs
Implements the parsing phase of compilation.
Jackson Eshbaugh
Written while following the book "Writing a C Compiler" by Nora Sandler.
*/

// Define AST data structs

pub trait PrettyPrint {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result;
}

pub struct Program {
    pub function_definition: Function
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

pub struct Function {
    pub identifier: Identifier,
    pub body: Statement
}

impl PrettyPrint for Function {
        fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
            let indent_str = "  ".repeat(indent);
            match self {
                Function {identifier, body} => {
                    writeln!(f, "{}Function(", indent_str)?;
                    identifier.pretty_print(f, indent + 1)?;
                    body.pretty_print(f, indent + 1)?;
                    writeln!(f, "{})", indent_str)
                }
            }
        }
    }

pub enum Statement {
    Return(Expression)
}

impl PrettyPrint for Statement {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            Return(exp) => {
                writeln!(f, "{}Return(", indent_str)?;
                exp.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
        }
    }
}

pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

impl PrettyPrint for Expression {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        match self {
            Constant(val) => {
                writeln!(f, "{}Constant({})", indent_str, val)
            },
            Unary(op, expr) => {
                write!(f, "{}Unary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                writeln!(f, ", ")?;
                expr.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            },
            Binary(op, left, right) => {
                write!(f, "{}Binary(", indent_str)?;
                op.pretty_print(f, indent + 1)?;
                write!(f, ", ")?;
                left.pretty_print(f, indent + 1)?;
                write!(f, ", ")?;
                right.pretty_print(f, indent + 1)?;
                writeln!(f, "{})", indent_str)
            }
        }
    }
}

pub enum UnaryOperator {
    Complement,
    Negate,
    Not
}

impl PrettyPrint for UnaryOperator {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        match self {
            Complement => write!(f, "Complement"),
            Negate => write!(f, "Negate"),
            Not => write!(f, "Not")
        }
    }
}

pub enum BinaryOperator {
    // Arithmetic
    Add, Subtract, Multiply, Divide, Remainder,

    // Bitwise
    BitwiseAnd, BitwiseOr, Xor, LeftShift, RightShift,

    // Relational
    And, Or, Equal, NotEqual, LessThan, LessOrEqual,
    GreaterThan, GreaterOrEqual
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

#[derive(Clone)]
pub struct Identifier {
    pub name: String
}

impl PrettyPrint for Identifier {
    fn pretty_print(&self, f: &mut Formatter, _indent: usize) -> fmt::Result {
        writeln!(f, "Identifier({})", self.name)
    }
}

/**
Entry point for parsing. Uses recursive descent.
*/
pub fn parse(tokens: &mut VecDeque<Token>) -> Result<Program, CompileError> {
    let function_definition = parse_function(tokens)?;

    if !tokens.is_empty() {
        return Err(CompileError::Syntax("Expected end of file, found more.".to_string()));
    }

    Ok(Program { function_definition })
}

/**
Parses a function definition. Expects the definition to be the next occurring AST element
in the set of tokens. Returns the resulting Function struct.
*/
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function, CompileError> {
    expect(Int, tokens)?;

    let identifier = parse_identifier(tokens)?;

    expect(OpenParen, tokens)?;
    expect(Void, tokens)?;
    expect(CloseParen, tokens)?;
    expect(OpenBrace, tokens)?;

    let statement = parse_statement(tokens)?;

    expect(CloseBrace, tokens)?;
    Ok(Function { identifier, body: statement })
}

/**
Parses a statement. Expects the statement to be the next occurring AST element in the list.
Returns the Statement.
*/
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, CompileError> {
    expect(TokenKind::Return, tokens)?;

    let expression = parse_expression(tokens, 0)?;

    expect(Semicolon, tokens)?;
    Ok(Return(expression))
}

/**
Parses an expression. Expects the expression to be the next occuring AST element in the list. Returns the Expression.
*/
fn parse_expression(tokens: &mut VecDeque<Token>, min_prec: i32) -> Result<Expression, CompileError> {
    // <expression> ::= <factor> | <exp> <binop> <exp>
    let mut left = parse_factor(tokens)?;

    loop {
        let proceed = match tokens.front() {
            Some(t) => precedence(t).map_or(false, |p| p >= min_prec),
            None => false,
        };

        if !proceed { break; }

        // Capture operator precedence in a short scope so the immutable borrow ends before mutation
        let op_prec: i32 = {
            let t = tokens.front().ok_or_else(|| CompileError::Syntax("Unexpected end of tokens.".to_string()))?;
            precedence(t).expect("internal: expected an operator token with precedence")
        };

        let operator = parse_binop(tokens)?;
        let right = parse_expression(tokens, op_prec + 1)?;
        left = Binary(operator, Box::new(left), Box::new(right));
    }

    Ok(left)
}

fn precedence(token: &Token) -> Option<i32> {
    use TokenKind::*;
    match token.kind {
        Asterisk | ForwardSlash | Percent => Some(50),
        Plus | Minus => Some(45),
        LeftShift | RightShift => Some(40),
        LessThan | LessEq | GreaterThan | GreaterEq => Some(35),
        EqualTo | NotEqual => Some(30),
        BitwiseAnd => Some(25),
        Xor => Some(24),
        BitwiseOr => Some(23),
        LogicalAnd => Some(20),
        LogicalOr => Some(15),
        _ => None,
    }
}

fn parse_binop(tokens: &mut VecDeque<Token>) -> Result<BinaryOperator, CompileError> {
    match tokens.pop_front() {
        Some(Token{ kind: TokenKind::Plus, value: _ }) => {
            Ok(BinaryOperator::Add)
        },
        Some(Token{ kind: TokenKind::Minus, value: _ }) => {
            Ok(BinaryOperator::Subtract)
        },
        Some(Token{ kind: TokenKind::Asterisk, value: _ }) => {
            Ok(BinaryOperator::Multiply)
        },
        Some(Token{ kind: TokenKind::ForwardSlash, value: _ }) => {
            Ok(BinaryOperator::Divide)
        },
        Some(Token{ kind: TokenKind::Percent, value: _ }) => {
            Ok(BinaryOperator::Remainder)
        },
        Some(Token{ kind: TokenKind::LeftShift, value: _ }) => {
            Ok(BinaryOperator::LeftShift)
        },
        Some(Token{ kind: TokenKind::RightShift, value: _ }) => {
            Ok(BinaryOperator::RightShift)
        },
        Some(Token{ kind: TokenKind::BitwiseAnd, value: _ }) => {
            Ok(BinaryOperator::BitwiseAnd)
        },
        Some(Token{ kind: TokenKind::BitwiseOr, value: _ }) => {
            Ok(BinaryOperator::BitwiseOr)
        },
        Some(Token{ kind: TokenKind::Xor, value: _ }) => {
            Ok(BinaryOperator::Xor)
        },
        Some(Token{ kind: TokenKind::LogicalAnd, value: _ }) => {
            Ok(BinaryOperator::And)
        },
        Some(Token{ kind: TokenKind::LogicalOr, value: _ }) => {
            Ok(BinaryOperator::Or)
        },
        Some(Token{ kind: TokenKind::EqualTo, value: _ }) => {
            Ok(BinaryOperator::Equal)
        },
        Some(Token{ kind: TokenKind::NotEqual, value: _ }) => {
            Ok(BinaryOperator::NotEqual)
        },
        Some(Token{ kind: TokenKind::LessThan, value: _ }) => {
            Ok(BinaryOperator::LessThan)
        },
        Some(Token{ kind: TokenKind::LessEq, value: _ }) => {
            Ok(BinaryOperator::LessOrEqual)
        },
        Some(Token{ kind: TokenKind::GreaterThan, value: _ }) => {
            Ok(BinaryOperator::GreaterThan)
        },
        Some(Token{ kind: TokenKind::GreaterEq, value: _ }) => {
            Ok(BinaryOperator::GreaterOrEqual)
        }
        Some(Token{ kind, value: _ }) => Err(CompileError::Syntax(String::from(format!("Invalid token ({}); expected binary operator.", kind)))),
        _ => Err(CompileError::Syntax(String::from("Unexpected end of tokens.")))
    }
}

/**
Parses a factor expression. Expects the factor to be the next occurring AST element in the list.
Returns the Expression.
*/
fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Expression, CompileError> {
    // <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"

    // Peek at the next token's kind in a short scope to avoid borrow conflicts
    let next_kind = {
        let t = tokens.front().ok_or_else(|| {
            CompileError::Syntax("Unexpected end of tokens.".to_string())
        })?;
        t.kind
    };

    match next_kind {
        TokenKind::Constant => Ok(Constant(parse_int(tokens)?)),

        TokenKind::BitwiseComplement => {
            tokens.pop_front();
            Ok(Unary(Complement, Box::new(parse_factor(tokens)?)))
        }

        TokenKind::Minus => {
            tokens.pop_front();
            Ok(Unary(Negate, Box::new(parse_factor(tokens)?)))
        }

        TokenKind::LogicalNot => {
            tokens.pop_front();
            Ok(Unary(Not, Box::new(parse_factor(tokens)?)))
        }

        TokenKind::OpenParen => {
            tokens.pop_front();
            let exp = parse_expression(tokens, 0)?;
            expect(CloseParen, tokens)?;
            Ok(exp)
        }

        _ => Err(CompileError::Syntax(format!(
            "Unexpected token for expression: {}",
            next_kind
        ))),
    }
}

/**
Parses an integer constant. Expects the next token to be the integer constant.
Returns the i32.
*/
fn parse_int(tokens: &mut VecDeque<Token>) -> Result<i32, CompileError> {
    // <int> ::= a constant token

    let token = tokens.pop_front().ok_or_else(|| {
        CompileError::Syntax("Unexpected end of tokens.".to_string())
    })?;

    let raw = token.value.ok_or_else(|| {
        CompileError::Syntax("Expected CONSTANT token to have a value, but found none.".to_string())
    })?;

    raw.parse::<i32>().map_err(|_| {
        CompileError::Syntax("Expected integer constant, but failed to parse.".to_string())
    })
}

fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Identifier, CompileError> {
    let token = tokens.pop_front().ok_or_else(||
        CompileError::Syntax("Unexpected end of tokens.".to_string())
    )?;

    if token.kind != IdentifierToken {
        return Err(CompileError::Syntax(format!(
            "Expected IDENTIFIER, but found '{}'", token.kind
        )));
    }

    match token.value {
        Some(name) => Ok(Identifier { name }),
        None => Err(CompileError::Syntax(
            "Expected IDENTIFIER token to have a value, but found none.".to_string()
        )),
    }
}

/// Ensure the next token is what we expect.
fn expect(kind: TokenKind, tokens: &mut VecDeque<Token>) -> Result<(), CompileError> {
    let actual = tokens.pop_front().ok_or_else(|| CompileError::Syntax("Unexpected end of tokens.".to_string()))?;
    if actual.kind != kind {
        return Err(CompileError::Syntax(format!("Expected {:?}, instead found {:?}", kind, actual.kind)));
    }
    Ok(())
}