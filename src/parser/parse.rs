use crate::compile_error::CompileError;
use crate::lexer::TokenKind::{Assignment, CloseBrace, CloseParen, IdentifierToken, Int, OpenBrace, OpenParen, Semicolon, Void};
use crate::lexer::{Token, TokenKind};
use crate::parser::Expression::{Constant, Unary};
use crate::parser::UnaryOperator::{Complement, Negate, Not};
use crate::parser::{BinaryOperator, BlockItem, Declaration, Expression, Function, Identifier, Program, Statement};
use std::collections::VecDeque;

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

    let mut function_body: Vec<BlockItem> = vec![];
    while tokens.front().map(|t| t.kind) != Some(CloseBrace) {
        function_body.push(parse_block_item(tokens)?);
    }

    expect(CloseBrace, tokens)?;

    Ok(Function { identifier, body: function_body })
}

fn parse_block_item(tokens: &mut VecDeque<Token>) -> Result<BlockItem, CompileError> {
    // <block item> ::= <statement> | <declaration>

    if tokens.front().unwrap().kind == Int {
        // declaration
        Ok(BlockItem::Declaration(parse_declaration(tokens)?))
    } else {
        // statement
        Ok(BlockItem::Statement(parse_statement(tokens)?))
    }

}

fn parse_declaration(tokens: &mut VecDeque<Token>) -> Result<Declaration, CompileError> {
    // <declaration> ::= "int" <identifier> [ "=" <expression> ] ";"

    // consume "int"
    expect(TokenKind::Int, tokens)?;

    // parse identifier
    let identifier = parse_identifier(tokens)?;

    // look ahead to decide which form we have
    match tokens.front().map(|t| t.kind) {
        Some(TokenKind::Semicolon) => {
            tokens.pop_front(); // consume ';'
            Ok(Declaration::Declaration(identifier, None))
        }

        Some(TokenKind::Assignment) => {
            tokens.pop_front(); // consume '='
            let init = parse_expression(tokens, 0)?;
            expect(TokenKind::Semicolon, tokens)?;
            Ok(Declaration::Declaration(identifier, Some(init)))
        }

        Some(other) => Err(CompileError::Syntax(format!(
            "Expected ';' or '=' after declaration of '{}', found {:?}.",
            identifier.name, other
        ))),

        None => Err(CompileError::Syntax(
            "Unexpected end of input in declaration.".to_string(),
        )),
    }
}

/**
Parses a statement. Expects the statement to be the next occurring AST element in the list.
Returns the Statement.
*/
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, CompileError> {
    match tokens.front().map(|t| t.kind) {
        Some(TokenKind::Return) => {
            tokens.pop_front();
            let e = parse_expression(tokens, 0)?;
            expect(Semicolon, tokens)?;
            Ok(Statement::Return(e))
        }
        Some(Semicolon) => {
            tokens.pop_front();
            Ok(Statement::Null)
        }
        Some(_) => {
            let e = parse_expression(tokens, 0)?;
            expect(Semicolon, tokens)?;
            Ok(Statement::Expression(e))
        }
        None => Err(CompileError::Syntax("Unexpected end of tokens in statement.".to_string())),
    }
}

/**
Parses an expression. Expects the expression to be the next occuring AST element in the list. Returns the Expression.
*/
fn parse_expression(tokens: &mut VecDeque<Token>, min_prec: i32) -> Result<Expression, CompileError> {
    let mut left = parse_factor(tokens)?;

    loop {
        let proceed = match tokens.front() {
            Some(t) => precedence(t).map_or(false, |p| p >= min_prec),
            None => false,
        };
        if !proceed { break; }

        if tokens.front().unwrap().kind == Assignment {
            // assignment is right-associative
            let op_tok = tokens.pop_front().unwrap();
            let op_prec = precedence(&op_tok).unwrap(); // should be the lowest precedence

            let right = parse_expression(tokens, op_prec)?; // NOTE: op_prec, not op_prec + 1
            left = Expression::Assignment(Box::new(left), Box::new(right));
        } else {
            let op_prec = {
                let t = tokens.front().ok_or_else(|| CompileError::Syntax("Unexpected end of tokens.".to_string()))?;
                precedence(t).expect("internal: expected an operator token with precedence")
            };

            let operator = parse_binop(tokens)?;
            let right = parse_expression(tokens, op_prec + 1)?;
            left = Expression::Binary(operator, Box::new(left), Box::new(right));
        }
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
        Assignment => Some(1),
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
    // <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"

    // Peek at the next token's kind in a short scope to avoid borrow conflicts
    let next_kind = {
        let t = tokens.front().ok_or_else(|| {
            CompileError::Syntax("Unexpected end of tokens.".to_string())
        })?;
        t.kind
    };

    match next_kind {
        TokenKind::Constant => Ok(Constant(parse_int(tokens)?)),

        TokenKind::IdentifierToken => {
            let id = parse_identifier(tokens)?;
            Ok(Expression::Variable(id))
        }

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