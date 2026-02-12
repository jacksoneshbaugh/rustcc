use crate::compile_error::CompileError;
use crate::lexer::TokenKind::{CloseBrace, CloseParen, Colon, IdentifierToken, Int, OpenBrace, OpenParen, QuestionMark, Semicolon, Void, While};
use crate::lexer::{Token, TokenKind};
use crate::parser::Expression::Ternary;
use crate::parser::Statement::DoWhile;
use crate::parser::{AssignOp, BinaryOperator, Block, BlockItem, Declaration, Expression, ForInit, Function, Identifier, Program, Statement, UnaryOperator};
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

    Ok(Function { identifier, body: Block { items: function_body } })
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


fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Block, CompileError> {
    // <block> ::= "{" { block item> } "}"
    expect(OpenBrace, tokens)?;
    let mut block_items: Vec<BlockItem> = vec![];
    while tokens.front().map(|t| t.kind) != Some(CloseBrace) {
        block_items.push(parse_block_item(tokens)?);
    }
    expect(CloseBrace, tokens)?;

    Ok(Block{ items: block_items })
}

/**
Parses a statement. Expects the statement to be the next occurring AST element in the list.
Returns the Statement.
*/
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, CompileError> {
    // <statement> ::= "return" <exp> ";"
    //               | <exp> ";"
    //               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
    //               | "break" ";"
    //               | "continue" ";"
    //               | "while" "(" <expresion> ")" <statement>
    //               | "do" <statement> "while" "(" <expression> ")" ";"
    //               | "for" "(" <for-init> [ <expression> ] ";" [ <expression> ] ")" <statement>
    //               | "goto" <identifier> ";"
    //               | <block>
    //               | ";"

    // label: <identifier> ":" <statement>
    if matches!(tokens.front().map(|t| t.kind), Some(IdentifierToken))
        && matches!(tokens.get(1).map(|t| t.kind), Some(Colon))
    {
        let lab = parse_identifier(tokens)?;   // consumes identifier
        expect(Colon, tokens)?;                         // consumes ':'
        let stmt = parse_statement(tokens)?;  // labeled statement is any statement
        return Ok(Statement::Label(lab, Box::new(stmt)));
    }

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
        },

        Some(TokenKind::Break) => {
            tokens.pop_front();
            expect(Semicolon, tokens)?;
            Ok(Statement::Break(None))
        },

        Some(TokenKind::Continue) => {
            tokens.pop_front();
            expect(Semicolon, tokens)?;
            Ok(Statement::Continue(None))
        },

        Some(TokenKind::If) => {
          tokens.pop_front(); // get rid of the "if"
            expect(OpenParen, tokens)?;
            let condition = parse_expression(tokens, 0)?;
            expect(CloseParen, tokens)?;
            let then = parse_statement(tokens)?;

            match tokens.front().map(|t| t.kind) {
                Some(TokenKind::Else) => {
                    tokens.pop_front(); // get rid of the "else"
                    let el = parse_statement(tokens)?;

                    Ok(Statement::If(condition, Box::new(then), Some(Box::new(el))))
                },
                _ => Ok(Statement::If(condition, Box::new(then), None)),
            }

        },

        Some(TokenKind::While) => {
            tokens.pop_front();                                          // while
            expect(OpenParen, tokens)?;                                         // (
            let condition = parse_expression(tokens, 0)?;    // <expression>
            expect(CloseParen, tokens)?;                                        // )
            let stmt = parse_statement(tokens)?;                      // <statement>

            Ok(Statement::While(condition, Box::new(stmt), None))
        },

        Some(TokenKind::Do) => {
            tokens.pop_front();                                          // do
            let stmt = parse_statement(tokens)?;                      // <statement>
            expect(While, tokens)?;                                             // while
            expect(OpenParen, tokens)?;                                         // (
            let condition = parse_expression(tokens, 0)?;    // <expresson>
            expect(CloseParen, tokens)?;                                        // )
            expect(Semicolon, tokens)?;                                         // ;

            Ok(DoWhile(Box::new(stmt), condition, None))
        },

        Some(TokenKind::For) => {
            tokens.pop_front();                                         // for
            expect(OpenParen, tokens)?;                                        // (
            let for_init = parse_for_init(tokens)?;                     // <for init>

            // condition
            let exp1 =
                if tokens.front().map(|t| t.kind) == Some(Semicolon) {
                None
            } else {
                Some(parse_expression(tokens, 0)?)
            };

            expect(Semicolon, tokens)?;

            // post
            let exp2 =
                if tokens.front().map(|t| t.kind) == Some(CloseParen) {
                None
            } else {
                Some(parse_expression(tokens, 0)?)
            };

            expect(CloseParen, tokens)?;

            let stmt = parse_statement(tokens)?;
            
            Ok(Statement::For(for_init, exp1, exp2, Box::new(stmt), None))
        }

        Some(OpenBrace) => Ok(Statement::Compound(parse_block(tokens)?)),

        Some(TokenKind::Goto) => {
            // goto <identifier>;

            tokens.pop_front(); // get rid of the goto
            let ident = parse_identifier(tokens)?;
            expect(Semicolon, tokens)?;

            Ok(Statement::Goto(ident))
        },

        Some(TokenKind::Switch) => {
            tokens.pop_front();
            expect(OpenParen, tokens)?;
            let condition = parse_expression(tokens, 0)?;
            expect(CloseParen, tokens)?;
            let stmt = parse_statement(tokens)?;

            Ok(Statement::Switch(condition, Box::new(stmt), None))
        }

        Some(TokenKind::Case) => {
            tokens.pop_front();
            let constant = parse_int(tokens)?;

            expect(Colon, tokens)?;

            let stmt = parse_statement(tokens)?;
            Ok(Statement::Case(constant, None, Box::new(stmt)))

        }

        Some(TokenKind::Default) => {
            tokens.pop_front();
            expect(Colon, tokens)?;
            let stmt = parse_statement(tokens)?;
            Ok(Statement::Default(None, Box::new(stmt)))
        }

        Some(_) => {
            let e = parse_expression(tokens, 0)?;
            expect(Semicolon, tokens)?;
            Ok(Statement::Expression(e))
        }
        None => Err(CompileError::Syntax("Unexpected end of tokens in statement.".to_string())),
    }
}

fn is_assignment_kind(k: TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        k,
        Assignment
            | AddAssign | SubtractAssign | MultiplyAssign | DivAssign | ModAssign
            | AndAssign | OrAssign | XorAssign
            | LeftShiftAssign | RightShiftAssign
    )
}

fn parse_assign_op(tokens: &mut VecDeque<Token>) -> Result<AssignOp, CompileError> {
    use TokenKind::*;
    let tok = tokens.pop_front().ok_or_else(|| CompileError::Syntax("Unexpected EOF".into()))?;
    let op = match tok.kind {
        Assignment => AssignOp::Assign,
        AddAssign => AssignOp::AddAssign,
        SubtractAssign => AssignOp::SubAssign,
        MultiplyAssign => AssignOp::MulAssign,
        DivAssign => AssignOp::DivAssign,
        ModAssign => AssignOp::ModAssign,
        AndAssign => AssignOp::AndAssign,
        OrAssign => AssignOp::OrAssign,
        XorAssign => AssignOp::XorAssign,
        LeftShiftAssign => AssignOp::ShlAssign,
        RightShiftAssign => AssignOp::ShrAssign,
        other => {
            return Err(CompileError::Syntax(format!(
                "Expected assignment operator, got {}",
                other
            )))
        }
    };
    Ok(op)
}

/**
Parses an expression. Expects the expression to be the next occuring AST element in the list. Returns the Expression.
*/
fn parse_expression(tokens: &mut VecDeque<Token>, min_prec: i32) -> Result<Expression, CompileError> {
    // <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>

    let mut left = parse_factor(tokens)?;

    loop {
        let proceed = match tokens.front() {
            Some(t) => precedence(t).map_or(false, |p| p >= min_prec),
            None => false,
        };
        if !proceed { break; }

        let k = tokens.front().unwrap().kind;

        if is_assignment_kind(k) {
            // right-associative: use op_prec
            let op_tok = tokens.front().cloned().unwrap();
            let op_prec = precedence(&op_tok).unwrap();

            let op = parse_assign_op(tokens)?;
            let right = parse_expression(tokens, op_prec)?; // right-assoc

            left = Expression::Assignment(op, Box::new(left), Box::new(right));
        } else if k == QuestionMark {
            // conditional operator
            tokens.pop_front(); // consume "?"
            let middle = parse_expression(tokens, 0)?;
            expect(Colon, tokens)?;

            let q_prec = precedence(&Token { kind: QuestionMark, value: None }).unwrap();
            let right = parse_expression(tokens, q_prec)?;
            left = Ternary(Box::new(left), Box::new(middle), Box::new(right));
        } else {
            let op_prec = {
                let t = tokens.front().ok_or_else(|| CompileError::Syntax("Unexpected EOF".into()))?;
                precedence(t).unwrap()
            };
            let operator = parse_binop(tokens)?;
            let right = parse_expression(tokens, op_prec + 1)?;
            left = Expression::Binary(operator, Box::new(left), Box::new(right));
        }
    }

    Ok(left)
}

fn parse_for_init(tokens: &mut VecDeque<Token>) -> Result<ForInit, CompileError> {
    // <for init> ::= <declaration> | [ <exp> ] ";"

    match tokens.front().map(|t| t.kind) {
        Some(Int) => Ok(ForInit::InitDeclaration(parse_declaration(tokens)?)),
        Some(Semicolon) => {
            expect(Semicolon, tokens)?; // pop semicolon
            Ok(ForInit::InitExpression(None))
        },
        Some(_) => {
            let expr = parse_expression(tokens, 0)?;
            expect(Semicolon, tokens)?;
            Ok(ForInit::InitExpression(Some(expr)))
        },
        None => Err(CompileError::Syntax("Expected loop parameters, got EOF.".to_string())),
    }

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

        QuestionMark => Some(3),

        // assignment family (all same, very low)
        Assignment
        | AddAssign | SubtractAssign | MultiplyAssign | DivAssign | ModAssign
        | AndAssign | OrAssign | XorAssign
        | LeftShiftAssign | RightShiftAssign => Some(1),

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
    use TokenKind::*;

    let next_kind = tokens
        .front()
        .ok_or_else(|| CompileError::Syntax("Unexpected end of tokens.".to_string()))?
        .kind;

    match next_kind {
        // literals
        Constant => Ok(Expression::Constant(parse_int(tokens)?)),

        // identifiers (possibly followed by postfix ++/--)
        IdentifierToken => {
            let id = parse_identifier(tokens)?;
            let mut expr = Expression::Variable(id);

            match tokens.front().map(|t| t.kind) {
                Some(Increment) => {
                    tokens.pop_front(); // consume '++'
                    expr = Expression::PostInc(Box::new(expr));
                }
                Some(Decrement) => {
                    tokens.pop_front(); // consume '--'
                    expr = Expression::PostDec(Box::new(expr));
                }
                _ => {}
            }

            Ok(expr)
        }

        // prefix ++/--
        Increment => {
            tokens.pop_front(); // consume '++'
            // must apply to a factor that is an lvalue (at least variable for now)
            let inner = parse_factor(tokens)?;
            Ok(Expression::PreInc(Box::new(inner)))
        }
        Decrement => {
            tokens.pop_front(); // consume '--'
            let inner = parse_factor(tokens)?;
            Ok(Expression::PreDec(Box::new(inner)))
        }

        // unary operators
        BitwiseComplement => {
            tokens.pop_front();
            Ok(Expression::Unary(
                UnaryOperator::Complement,
                Box::new(parse_factor(tokens)?),
            ))
        }
        Minus => {
            tokens.pop_front();
            Ok(Expression::Unary(
                UnaryOperator::Negate,
                Box::new(parse_factor(tokens)?),
            ))
        }
        LogicalNot => {
            tokens.pop_front();
            Ok(Expression::Unary(
                UnaryOperator::Not,
                Box::new(parse_factor(tokens)?),
            ))
        }

        // parenthesized expression (optionally followed by postfix ++/--)
        OpenParen => {
            tokens.pop_front();
            let inner = parse_expression(tokens, 0)?;
            expect(CloseParen, tokens)?;

            // If you want to allow `(x)++` etc., support postfix here too:
            match tokens.front().map(|t| t.kind) {
                Some(Increment) => {
                    tokens.pop_front();
                    Ok(Expression::PostInc(Box::new(inner)))
                }
                Some(Decrement) => {
                    tokens.pop_front();
                    Ok(Expression::PostDec(Box::new(inner)))
                }
                _ => Ok(inner),
            }
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
    let token = tokens.pop_front().ok_or_else(|| {
        CompileError::Syntax("Unexpected end of tokens.".to_string())
    })?;

    let raw = token.value.ok_or_else(|| {
        CompileError::Syntax("Expected CONSTANT token to have a value, but found none.".to_string())
    })?;

    // C integer literals: 0x... (hex), 0... (octal), otherwise decimal.
    let s = raw.trim();

    let parsed: i32 = if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        i32::from_str_radix(hex, 16).map_err(|_| {
            CompileError::Syntax(format!("Invalid hex integer literal '{}'.", s))
        })?
    } else if s.len() > 1 && s.starts_with('0') {
        // treat as octal (C rule for leading-0 integer constants)
        i32::from_str_radix(&s[1..], 8).map_err(|_| {
            CompileError::Syntax(format!("Invalid octal integer literal '{}'.", s))
        })?
    } else {
        s.parse::<i32>().map_err(|_| {
            CompileError::Syntax(format!("Invalid decimal integer literal '{}'.", s))
        })?
    };

    Ok(parsed)
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