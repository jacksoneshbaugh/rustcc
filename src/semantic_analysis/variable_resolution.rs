use std::collections::HashMap;
use crate::compile_error::CompileError;
use crate::parser::{BlockItem, Declaration, Expression, Function, Identifier, Program, Statement};

/**
rustcc | semantic_analysis/variable_resolution.rs
Contains code pertaining to the variable resolution pass of
semantic analysis

Jackson Eshbaugh
Written while following "Writing a C Compiler" by Nora Sandler
*/

pub fn resolve_program(program: Program) -> Result<Program, CompileError> {
    let mut variable_map: HashMap<Identifier, Identifier> = HashMap::new();
    let mut name_gen = NameGen::new();

    let function_definition =
        resolve_function(program.function_definition, &mut variable_map, &mut name_gen)?;

    Ok(Program { function_definition })
}

fn resolve_function(
    function: Function,
    variable_map: &mut HashMap<Identifier, Identifier>,
    name_gen: &mut NameGen,
) -> Result<Function, CompileError> {

    let mut new_body = Vec::new();

    for item in function.body {
        let resolved_item = match item {
            BlockItem::Declaration(declaration) => {
                BlockItem::Declaration(
                    resolve_declaration(declaration, variable_map, name_gen)?
                )
            }

            BlockItem::Statement(statement) => {
                BlockItem::Statement(
                    resolve_statement(statement, variable_map, name_gen)?
                )
            }
        };

        new_body.push(resolved_item);
    }

    Ok(Function {
        identifier: function.identifier,
        body: new_body,
    })
}

fn resolve_declaration(
    declaration: Declaration,
    variable_map: &mut HashMap<Identifier, Identifier>,
    name_gen: &mut NameGen,
) -> Result<Declaration, CompileError> {
    match declaration {
        Declaration::Declaration(id, None) => {
            if variable_map.contains_key(&id) {
                return Err(CompileError::Semantic(format!(
                    "Duplicate variable {} declared.",
                    id.name
                )));
            }

            let unique_name = name_gen.fresh_temp_ident();
            variable_map.insert(id, unique_name.clone());

            Ok(Declaration::Declaration(unique_name, None))
        }

        Declaration::Declaration(id, Some(init_expr)) => {
            if variable_map.contains_key(&id) {
                return Err(CompileError::Semantic(format!(
                    "Duplicate variable {} declared.",
                    id.name
                )));
            }

            let unique_name = name_gen.fresh_temp_ident();
            variable_map.insert(id, unique_name.clone());

            let resolved_init = resolve_expression(init_expr, variable_map, name_gen)?;
            Ok(Declaration::Declaration(unique_name, Some(resolved_init)))
        }
    }
}

fn resolve_statement(
    statement: Statement,
    variable_map: &mut HashMap<Identifier, Identifier>,
    name_gen: &mut NameGen,
) -> Result<Statement, CompileError> {
    match statement {
        Statement::Return(e) => Ok(Statement::Return(resolve_expression(e, variable_map, name_gen)?)),
        Statement::Expression(e) => Ok(Statement::Expression(resolve_expression(e, variable_map, name_gen)?)),
        Statement::Null => Ok(Statement::Null),
    }
}

fn resolve_expression(
    expression: Expression,
    variable_map: &mut HashMap<Identifier, Identifier>,
    name_gen: &mut NameGen,
) -> Result<Expression, CompileError> {
    Ok(match expression {
        Expression::Assignment(left, right) => {
            let lhs_ident = match *left {
                Expression::Variable(id) => id,
                _ => {
                    return Err(CompileError::Syntax(
                        "Left-hand side of assignment must be a variable".to_string(),
                    ));
                }
            };

            let new_lhs = resolve_identifier(lhs_ident, variable_map)?;
            let new_rhs = resolve_expression(*right, variable_map, name_gen)?;

            Expression::Assignment(
                Box::new(Expression::Variable(new_lhs)),
                Box::new(new_rhs),
            )
        }

        Expression::Variable(id) => Expression::Variable(resolve_identifier(id, variable_map)?),

        Expression::Unary(op, inner) => {
            let inner = resolve_expression(*inner, variable_map, name_gen)?;
            Expression::Unary(op, Box::new(inner))
        }

        Expression::Binary(op, left, right) => {
            let left = resolve_expression(*left, variable_map, name_gen)?;
            let right = resolve_expression(*right, variable_map, name_gen)?;
            Expression::Binary(op, Box::new(left), Box::new(right))
        }

        Expression::Constant(n) => Expression::Constant(n),
    })
}

fn resolve_identifier(
    id: Identifier,
    variable_map: &HashMap<Identifier, Identifier>,
) -> Result<Identifier, CompileError> {
    variable_map
        .get(&id)
        .cloned()
        .ok_or_else(|| CompileError::Syntax(format!("Variable {} not found.", id.name)))
}

#[derive(Default)]
pub struct NameGen {
    next: u64,
}

impl NameGen {
    pub fn new() -> Self {
        NameGen { next: 0 }
    }

    pub fn fresh_temp(&mut self) -> String {
        let id = self.next;
        self.next += 1;
        format!("_tmp{}", id)
    }

    pub fn fresh_temp_ident(&mut self) -> Identifier {
        Identifier { name: self.fresh_temp() }
    }
}