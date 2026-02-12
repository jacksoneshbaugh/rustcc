use std::collections::HashSet;

use crate::compile_error::CompileError;
use crate::parser::{Block, BlockItem, Function, Identifier, Program, Statement};

pub fn resolve_labels(program: Program) -> Result<Program, CompileError> {
    let function_definition = resolve_function_labels(program.function_definition)?;
    Ok(Program { function_definition })
}

fn resolve_function_labels(function: Function) -> Result<Function, CompileError> {
    // Pass 1: collect label definitions (function-scoped)
    let mut defined: HashSet<Identifier> = HashSet::new();
    collect_labels_block(&function.body, &mut defined)?;

    // Pass 2: validate gotos
    validate_gotos_block(&function.body, &defined)?;

    Ok(function)
}

fn collect_labels_block(block: &Block, defined: &mut HashSet<Identifier>) -> Result<(), CompileError> {
    for item in &block.items {
        collect_labels_block_item(item, defined)?;
    }
    Ok(())
}

fn collect_labels_block_item(
    item: &BlockItem,
    defined: &mut HashSet<Identifier>,
) -> Result<(), CompileError> {
    match item {
        BlockItem::Declaration(_) => Ok(()),
        BlockItem::Statement(stmt) => collect_labels_stmt(stmt, defined),
    }
}

fn collect_labels_stmt(
    stmt: &Statement,
    defined: &mut HashSet<Identifier>,
) -> Result<(), CompileError> {
    match stmt {
        Statement::Label(id, inner) => {
            // Labels are function-scoped; duplicates are illegal
            if !defined.insert(id.clone()) {
                return Err(CompileError::Semantic(format!(
                    "Duplicate label '{}' defined in function.",
                    id.name
                )));
            }
            collect_labels_stmt(inner, defined)
        }

        Statement::If(_cond, then_s, else_s) => {
            collect_labels_stmt(then_s, defined)?;
            if let Some(e) = else_s.as_deref() {
                collect_labels_stmt(e, defined)?;
            }
            Ok(())
        }

        Statement::Compound(block) => collect_labels_block(block, defined),

        Statement::While(_exp, body, _) => {
            collect_labels_stmt(body, defined)?;
            Ok(())
        }
        Statement::DoWhile(body, _exp, _) => {
            collect_labels_stmt(body, defined)?;
            Ok(())
        }
        Statement::For(_init, _exp1, _exp2, body, _) => {
            collect_labels_stmt(body, defined)?;
            Ok(())
        },

        Statement::Switch(_exp, b_stmt, _o_ident) => {
            collect_labels_stmt(b_stmt, defined)?;
            Ok(())
        }

        Statement::Case(_i, _ident, b_stmt) => {
            collect_labels_stmt(b_stmt, defined)?;
            Ok(())
        }
        Statement::Default(_ident, stmt) => {
            collect_labels_stmt(stmt, defined)?;
            Ok(())
        }

        // base cases
        Statement::Goto(_) | Statement::Return(_) | Statement::Expression(_) | Statement::Break(_)
        | Statement::Continue(_) | Statement::Null => Ok(()),
    }
}

fn validate_gotos_block(
    block: &Block,
    defined: &HashSet<Identifier>,
) -> Result<(), CompileError> {
    for item in &block.items {
        validate_gotos_block_item(item, defined)?;
    }
    Ok(())
}

fn validate_gotos_block_item(
    item: &BlockItem,
    defined: &HashSet<Identifier>,
) -> Result<(), CompileError> {
    match item {
        BlockItem::Declaration(_) => Ok(()),
        BlockItem::Statement(stmt) => validate_gotos_stmt(stmt, defined),
    }
}

fn validate_gotos_stmt(
    stmt: &Statement,
    defined: &HashSet<Identifier>,
) -> Result<(), CompileError> {
    match stmt {
        Statement::Goto(id) => {
            if !defined.contains(id) {
                return Err(CompileError::Semantic(format!(
                    "Goto to undefined label '{}'.",
                    id.name
                )));
            }
            Ok(())
        }

        Statement::Label(_id, inner) => validate_gotos_stmt(inner, defined),

        Statement::If(_cond, then_s, else_s) => {
            validate_gotos_stmt(then_s, defined)?;
            if let Some(e) = else_s.as_deref() {
                validate_gotos_stmt(e, defined)?;
            }
            Ok(())
        }

        Statement::Compound(block) => validate_gotos_block(block, defined),

        Statement::While(_exp, body, _) => {
            validate_gotos_stmt(body, defined)?;
            Ok(())
        }
        Statement::DoWhile(body, _exp, _) => {
            validate_gotos_stmt(body, defined)?;
            Ok(())
        }
        Statement::For(_init, _exp1, _exp2, body, _) => {
            validate_gotos_stmt(body, defined)?;
            Ok(())
        }

        Statement::Switch(_exp, b_stmt, _o_ident) => {
            validate_gotos_stmt(b_stmt, defined)?;
            Ok(())
        }

        Statement::Case(_i, _ident, b_stmt) => {
            validate_gotos_stmt(b_stmt, defined)?;
            Ok(())
        }
        Statement::Default(_ident, stmt) => {
            validate_gotos_stmt(stmt, defined)?;
            Ok(())
        }

        Statement::Continue(_) | Statement::Break(_) | Statement::Return(_) | Statement::Expression(_)
        | Statement::Null => Ok(()),
    }
}