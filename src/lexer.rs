/**
rustcc | lexer.rs
Holds the lexing compilation pass and related structures.
Jackson Eshbaugh
Written while following the book "Writing a C Compiler" by Nora Sandler.
*/

use regex::Regex;
use crate::compile_error::CompileError;
use crate::lexer::TokenKind::{Assignment, Asterisk, BitwiseAnd, BitwiseComplement, BitwiseOr, CloseBrace, CloseParen, Constant, Decrement, EqualTo, ForwardSlash, GreaterEq, GreaterThan, IdentifierToken, Int, LeftShift, LessEq, LessThan, LogicalAnd, LogicalNot, LogicalOr, Minus, NotEqual, OpenBrace, OpenParen, Percent, Plus, Return, RightShift, Semicolon, Void, Xor};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // keywords
    Int, Void, Return,

    // atoms
    IdentifierToken, Constant,

    // operators (multi)
    Decrement, LogicalAnd, LogicalOr, LeftShift, RightShift,
    LessEq, GreaterEq, NotEqual, EqualTo,

    // operators (single)
    LogicalNot, LessThan, GreaterThan, BitwiseAnd, BitwiseOr,
    Xor, Plus, Minus, Asterisk, ForwardSlash, Percent,
    BitwiseComplement, Assignment,

    // punctuation
    OpenParen, CloseParen, OpenBrace, CloseBrace, Semicolon,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        let s = match self {
            // keywords
            Int => "int",
            Void => "void",
            Return => "return",

            // atoms
            IdentifierToken => "identifier",
            Constant => "constant",

            // multi-char operators
            Decrement => "--",
            LogicalAnd => "&&",
            LogicalOr => "||",
            LeftShift => "<<",
            RightShift => ">>",
            LessEq => "<=",
            GreaterEq => ">=",
            EqualTo => "==",
            NotEqual => "!=",

            // single-char operators
            LogicalNot => "!",
            LessThan => "<",
            GreaterThan => ">",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            Xor => "^",
            Plus => "+",
            Minus => "-",
            Asterisk => "*",
            ForwardSlash => "/",
            Percent => "%",
            BitwiseComplement => "~",
            Assignment => "=",

            // punctuation
            OpenParen => "(",
            CloseParen => ")",
            OpenBrace => "{",
            CloseBrace => "}",
            Semicolon => ";",
        };
        write!(f, "{}", s)
    }
}

struct TokenDef {
    kind: TokenKind,
    pattern: Regex
}

pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Token{
            kind: self.kind,
            value: self.value.clone()
        }
    }
}

/**
Implements the lexing phase of the compiler. This takes in the path to a
file and tokenizes the result, returning either a list of tokens or a CompileError
if an unknown token is encountered.
*/
pub fn lex(file: &str) -> Result<Vec<Token>, CompileError> {
    let token_defs: Vec<TokenDef> = vec![
        // keywords OR do the identifier-remap approach (not both)
        TokenDef { kind: Int, pattern: Regex::new(r"^int\b").unwrap() },
        TokenDef { kind: Void, pattern: Regex::new(r"^void\b").unwrap() },
        TokenDef { kind: Return, pattern: Regex::new(r"^return\b").unwrap() },
        TokenDef { kind: IdentifierToken, pattern: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap() },
        TokenDef { kind: Constant, pattern: Regex::new(r"^[0-9]+\b").unwrap() },

        // multi-char operators first
        TokenDef { kind: Decrement, pattern: Regex::new(r"^--").unwrap() },
        TokenDef { kind: LogicalAnd, pattern: Regex::new(r"^&&").unwrap() },
        TokenDef { kind: LogicalOr, pattern: Regex::new(r"^\|\|").unwrap() },
        TokenDef { kind: LeftShift, pattern: Regex::new(r"^<<").unwrap() },
        TokenDef { kind: RightShift, pattern: Regex::new(r"^>>").unwrap() },
        TokenDef { kind: LessEq, pattern: Regex::new(r"^<=").unwrap() },
        TokenDef { kind: GreaterEq, pattern: Regex::new(r"^>=").unwrap() },
        TokenDef { kind: NotEqual, pattern: Regex::new(r"^!=").unwrap() },
        TokenDef { kind: EqualTo, pattern: Regex::new(r"^==").unwrap() },

        // single-char operators
        TokenDef { kind: LogicalNot, pattern: Regex::new(r"^!").unwrap() },
        TokenDef { kind: LessThan, pattern: Regex::new(r"^<").unwrap() },
        TokenDef { kind: GreaterThan, pattern: Regex::new(r"^>").unwrap() },
        TokenDef { kind: BitwiseAnd, pattern: Regex::new(r"^&").unwrap() },
        TokenDef { kind: BitwiseOr, pattern: Regex::new(r"^\|").unwrap() },
        TokenDef { kind: Xor, pattern: Regex::new(r"^\^").unwrap() },
        TokenDef { kind: Plus, pattern: Regex::new(r"^\+").unwrap() },
        TokenDef { kind: Minus, pattern: Regex::new(r"^-").unwrap() },
        TokenDef { kind: Asterisk, pattern: Regex::new(r"^\*").unwrap() },
        TokenDef { kind: ForwardSlash, pattern: Regex::new(r"^/").unwrap() },
        TokenDef { kind: Percent, pattern: Regex::new(r"^%").unwrap() },
        TokenDef { kind: BitwiseComplement, pattern: Regex::new(r"^~").unwrap() },
        TokenDef { kind: Assignment, pattern: Regex::new(r"^=").unwrap() },

        // punctuation
        TokenDef { kind: OpenParen, pattern: Regex::new(r"^\(").unwrap() },
        TokenDef { kind: CloseParen, pattern: Regex::new(r"^\)").unwrap() },
        TokenDef { kind: OpenBrace, pattern: Regex::new(r"^\{").unwrap() },
        TokenDef { kind: CloseBrace, pattern: Regex::new(r"^}").unwrap() },
        TokenDef { kind: Semicolon, pattern: Regex::new(r"^;").unwrap() },
    ];

    // Read from the file
    let contents = std::fs::read_to_string(&file)?;
    let mut input = contents.as_str();
    let mut tokens: Vec<Token> = Vec::new();

    let ws_re = Regex::new(r"^\s+").unwrap();

    while !input.is_empty() {
        // Skip leading whitespace
        if let Some(ws_match) = ws_re.find(input) {
            input = &input[ws_match.end()..];
            continue;
        }

        if let Some((def, m)) = token_defs
            .iter()
            .find_map(|def| def.pattern.find(input).map(|m| (def, m)))
        {
            let kind = def.kind;
            let text = m.as_str();

            let value = match kind {
                Int | Void | Return => None,
                _ => Some(text.to_string()),
            };

            tokens.push(Token { kind, value });
            input = &input[m.end()..];
        } else {
            eprintln!("Unrecognized token starting with: {:?}", input);

            return Err(CompileError::Syntax("Unexpected token found.".to_string()));
        }
    }

    Ok(tokens)
}