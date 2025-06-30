/**
rustcc | lexer.rs
Holds the lexing compilation pass and related structures.
Jackson Eshbaugh
Written while following the book "Writing a C Compiler" by Nora Sandler.
*/

use regex::Regex;
use crate::compile_error::CompileError;

struct TokenDef {
    kind: &'static str,
    pattern: Regex
}

pub struct Token {
    pub kind: &'static str,
    pub value: Option<String>
}

/**
Implements the lexing phase of the compiler. This takes in the path to a
file and tokenizes the result, returning either a list of tokens or a CompileError
if an unknown token is encountered.
*/
pub fn lex(file: &str) -> Result<Vec<Token>, CompileError> {

    let token_defs: Vec<TokenDef> = vec![
        TokenDef{
            kind: "IDENTIFIER",
            pattern: Regex::new(r"[a-zA-Z_]\w*\b").unwrap()
        },
        TokenDef{
            kind: "CONSTANT",
            pattern: Regex::new(r"[0-9]+\b").unwrap()
        },
        TokenDef{
            kind: "INT_KEYWORD",
            pattern: Regex::new(r"int\b").unwrap()
        },
        TokenDef{
            kind: "VOID_KEYWORD",
            pattern: Regex::new(r"void\b").unwrap()
        },
        TokenDef{
            kind: "RETURN_KEYWORD",
            pattern: Regex::new(r"return\b").unwrap()
        },
        TokenDef{
            kind: "OPEN_PARENTHESIS",
            pattern: Regex::new(r"\(").unwrap()
        },
        TokenDef{
            kind: "CLOSE_PARENTHESIS",
            pattern: Regex::new(r"\)").unwrap()
        },
        TokenDef{
            kind: "OPEN_BRACE",
            pattern: Regex::new(r"\{").unwrap()
        },
        TokenDef{
            kind: "CLOSE_BRACE",
            pattern: Regex::new(r"}").unwrap()
        },
        TokenDef{
            kind: "SEMICOLON",
            pattern: Regex::new(r";").unwrap()
        }
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

        let mut best_match_kind: Option<&str> = None;
        let mut best_match_value: Option<&str> = None;
        let mut best_match_len: usize = 0;

        for def in &token_defs {
            if let Some(m) = def.pattern.find(input) {
                // Match must begin at the start, and the end must be larger than or as large as the
                // current best match end. In other words, find the largest match anchored to the beginning
                // of the input.
                if m.start() == 0 && m.end() >= best_match_len {
                    best_match_kind = Some(def.kind);
                    best_match_value = Some(m.as_str());
                    best_match_len = m.end();
                }
            }
        }

        if let (Some(kind), Some(value)) = (best_match_kind, best_match_value) {

            if kind == "IDENTIFIER" {
                if value == "int" {
                    tokens.push(Token { kind: "INT_KEYWORD", value: None });
                } else if value == "void" {
                    tokens.push(Token { kind: "VOID_KEYWORD", value: None });
                } else if value == "return" {
                    tokens.push(Token { kind: "RETURN_KEYWORD", value: None });
                } else {
                    tokens.push(Token { kind, value: Some(value.to_string()) });
                }

                input = &input[best_match_len..];
            } else {

                tokens.push(Token {
                    kind,
                    value: Some(value.to_string()),
                });
                input = &input[best_match_len..]; // Advance input by match length
            }

        } else {
            eprintln!("Unrecognized token starting with: {:?}", input);
            return Err(CompileError::Syntax("Unexpected token found.".to_string()));
        }
    }

    Ok(tokens)
}