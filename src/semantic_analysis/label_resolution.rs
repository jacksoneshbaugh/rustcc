use std::collections::HashSet;

use crate::compile_error::CompileError;
use crate::parser::{BlockItem, Function, Program, Statement};

pub fn resolve_labels(program: Program) -> Result<Program, CompileError> {
    let function_definition = resolve_function_labels(program.function_definition)?;
    Ok(Program { function_definition })
}

fn resolve_function_labels(function: Function) -> Result<Function, CompileError> {
    // Pass 1: collect label definitions (and catch duplicates)
    let mut defined: HashSet<String> = HashSet::new();
    for item in &function.body {
        collect_labels_block_item(item, &mut defined)?;
    }

    // Pass 2: validate gotos (must target a defined label)
    for item in &function.body {
        validate_gotos_block_item(item, &defined)?;
    }

    Ok(function)
}

fn collect_labels_block_item(
    item: &BlockItem,
    defined: &mut HashSet<String>,
) -> Result<(), CompileError> {
    match item {
        BlockItem::Declaration(_) => Ok(()),
        BlockItem::Statement(stmt) => collect_labels_stmt(stmt, defined),
    }
}

fn collect_labels_stmt(
    stmt: &Statement,
    defined: &mut HashSet<String>,
) -> Result<(), CompileError> {
    match stmt {
        Statement::Label(id, inner) => {
            // labels are function-scoped; duplicate definitions are illegal
            if !defined.insert(id.name.clone()) {
                return Err(CompileError::Semantic(format!(
                    "Duplicate label '{}' defined in function.",
                    id.name
                )));
            }
            collect_labels_stmt(inner, defined)
        }

        // recurse into other statement forms if you have them
        Statement::If(cond, then_s, else_s) => {
            // condition contains no labels, but keep structure consistent
            let _ = cond;
            collect_labels_stmt(then_s, defined)?;
            if let Some(e) = else_s.as_deref() {
                collect_labels_stmt(e, defined)?;
            }
            Ok(())
        }

        // base cases
        Statement::Goto(_) | Statement::Return(_) | Statement::Expression(_) | Statement::Null => Ok(()),
    }
}

fn validate_gotos_block_item(
    item: &BlockItem,
    defined: &HashSet<String>,
) -> Result<(), CompileError> {
    match item {
        BlockItem::Declaration(_) => Ok(()),
        BlockItem::Statement(stmt) => validate_gotos_stmt(stmt, defined),
    }
}

fn validate_gotos_stmt(stmt: &Statement, defined: &HashSet<String>) -> Result<(), CompileError> {
    match stmt {
        Statement::Goto(id) => {
            if !defined.contains(&id.name) {
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

        Statement::Return(_) | Statement::Expression(_) | Statement::Null => Ok(()),
    }
}