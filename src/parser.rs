use crate::compile_error::CompileError;
use crate::lexer::Token;
use crate::parser::Expression::{Constant, Unary};
use crate::parser::Statement::Return;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use crate::parser::UnaryOperator::{Compliment, Negate};

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
            }
        }
    }
}

pub enum UnaryOperator {
    Compliment,
    Negate
}

impl PrettyPrint for UnaryOperator {
    fn pretty_print(&self, f: &mut Formatter, indent: usize) -> fmt::Result {
        match self {
            UnaryOperator::Compliment => write!(f, "Compliment"),
            UnaryOperator::Negate => write!(f, "Negate")
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

    // IMPLEMENTED ABSTRACT SYNTAX:
    // <program> ::= <function>

    let function_definition = match parse_function(tokens) {
        Ok(f) => f,
        Err(e) => return Err(e) // Parsing the definition failed.
    };

    if !tokens.is_empty() {
        return Err(CompileError::Syntax(String::from("Expected end of file, found more.")))
    }

    Ok(Program { function_definition })
}

/**
Parses a function definition. Expects the definition to be the next occurring AST element
in the set of tokens. Returns the resulting Function struct.
*/
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function, CompileError> {

    // IMPLEMENTED ABSTRACT SYNTAX:
    // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"

    match expect("INT_KEYWORD", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    let identifier = match parse_identifier(tokens) {
        Ok(i) => i,
        Err(e) => return Err(e)
    };

    match expect("OPEN_PARENTHESIS", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    match expect("VOID_KEYWORD", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    match expect("CLOSE_PARENTHESIS", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    match expect("OPEN_BRACE", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    let statement = match parse_statement(tokens) {
        Ok(e) => e,
        Err(e) => return Err(e)
    };

    match expect("CLOSE_BRACE", tokens) {
        Ok(_b) => Ok(Function { identifier, body: statement }),
        Err(e) => return Err(e)
    }
}

/**
Parses a statement. Expects the statement to be the next occurring AST element in the list.
Returns the Statement.
*/
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, CompileError> {

    // <statement> ::= "return" <expression> ";"

    match expect("RETURN_KEYWORD", tokens) {
        Ok(_b) => {},
        Err(e) => return Err(e)
    }

    let expression = match parse_expression(tokens) {
        Ok(e) => e,
        Err(e) => return Err(e)
    };

    match expect("SEMICOLON", tokens) {
        Ok(_b) => Ok(Return(expression)),
        Err(e) => return Err(e)
    }
}

/**
Parses an expression. Expects the expression to be the next occurring AST element in the list.
Returns the Expression.
*/
fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Expression, CompileError> {
    // <expression> ::= <int> | <unop> <int> | "(" <exp> ")"

    let token = tokens.front().unwrap();

    match token.kind {
        "CONSTANT" => Ok(Constant(parse_int(tokens)?)),
        "BITWISE_COMPLIMENT_OPERATOR" => {
            // <unary> ::= "~" <exp>
            tokens.pop_front();
            Ok(Unary(Compliment, Box::new(parse_expression(tokens)?)))
        },
        "NEGATION_SUBTRACTION_OPERATOR" => {
            // <unary> ::= "-" <exp>
            tokens.pop_front();
            Ok(Unary(Negate, Box::new(parse_expression(tokens)?)))
        },
        "OPEN_PARENTHESIS" => {
            // <exp> ::= "(" <exp> ")"
            
            tokens.pop_front();
            
            let exp = match parse_expression(tokens) {
                Ok(expr) => expr,
                Err(e) => return Err(e)
            };

            match expect("CLOSE_PARENTHESIS", tokens) {
                Ok(_b) => Ok(exp),
                Err(e) => Err(e)
            }
        },
        _ => Err(CompileError::Syntax(String::from(format!("Unexpected token for expression: {}", token.kind))))
    }
}

/**
Parses an integer constant. Expects the next token to be the integer constant.
Returns the i32.
*/
fn parse_int(tokens: &mut VecDeque<Token>) -> Result<i32, CompileError> {
    // <int> ::= a constant token

    let token = tokens.pop_front().ok_or_else(||
        CompileError::Syntax("Unexpected end of tokens.".to_string())
    )?;

    match token.value.unwrap().parse::<i32>() {
        Ok(i) => Ok(i),
        Err(_e) => return Err(CompileError::Syntax("Expected integer constant, but failed to parse.".parse().unwrap()))
    }

}

fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Identifier, CompileError> {
    let token = tokens.pop_front().ok_or_else(||
        CompileError::Syntax("Unexpected end of tokens.".to_string())
    )?;

    if token.kind != "IDENTIFIER" {
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
fn expect(token_kind: &str, tokens: &mut VecDeque<Token>) -> Result<bool, CompileError> {
    let actual = tokens.pop_front().ok_or_else(||
    CompileError::Syntax("Unexpected end of tokens.".to_string())
    )?;
    
    if actual.kind != token_kind {
       return Err(CompileError::Syntax(String::from(format!("Expected '{}', instead found '{}'", token_kind, actual.kind))))
    }
    Ok(actual.kind == token_kind)
}