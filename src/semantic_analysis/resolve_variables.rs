// rustcc | semantic_analysis/variable_resolution.rs
// Variable resolution (α-renaming) pass: assigns each declared variable a unique internal name
// and rewrites all variable uses to that unique name, respecting C block scoping.
//
// Jackson Eshbaugh — following "Writing a C Compiler" by Nora Sandler

use crate::compile_error::CompileError;
use crate::parser::{
    Block, BlockItem, Declaration, Expression, ForInit, Function, Identifier, Program, Statement,
};
use std::collections::HashMap;

// Map original source identifiers -> unique internal identifiers, scoped by block.
type Scope = HashMap<Identifier, Identifier>;
type Scopes = Vec<Scope>;

pub fn resolve_variables(program: Program) -> Result<Program, CompileError> {
    let mut name_gen = NameGen::new();
    let function_definition = resolve_function(program.function_definition, &mut name_gen)?;
    Ok(Program {
        function_definition,
    })
}

fn resolve_function(func: Function, name_gen: &mut NameGen) -> Result<Function, CompileError> {
    // Function scope
    let mut scopes: Scopes = vec![HashMap::new()];

    // don't push a new scope for the function body block;
    // the function-body block *is* the function scope.
    let body = resolve_block(func.body, &mut scopes, name_gen, /*push_scope=*/ false)?;

    Ok(Function {
        identifier: func.identifier,
        body,
    })
}

fn resolve_block(
    block: Block,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
    push_scope: bool,
) -> Result<Block, CompileError> {
    if push_scope {
        scopes.push(HashMap::new());
    }

    let mut new_items = Vec::with_capacity(block.items.len());
    for item in block.items {
        let resolved = match item {
            BlockItem::Declaration(d) => {
                BlockItem::Declaration(resolve_declaration(d, scopes, name_gen)?)
            }
            BlockItem::Statement(s) => {
                BlockItem::Statement(resolve_statement(s, scopes, name_gen)?)
            }
        };
        new_items.push(resolved);
    }

    if push_scope {
        scopes.pop();
    }

    Ok(Block { items: new_items })
}

fn resolve_declaration(
    d: Declaration,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
) -> Result<Declaration, CompileError> {
    match d {
        Declaration::Declaration(id, init) => {
            // Declare into the current scope only (shadowing allowed across scopes).
            let unique = declare(id, scopes, name_gen)?;

            let init = match init {
                Some(e) => Some(resolve_expression(e, scopes, name_gen)?),
                None => None,
            };

            Ok(Declaration::Declaration(unique, init))
        }
    }
}

fn resolve_statement(
    s: Statement,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
) -> Result<Statement, CompileError> {
    Ok(match s {
        Statement::Return(e) => Statement::Return(resolve_expression(e, scopes, name_gen)?),

        Statement::Expression(e) => Statement::Expression(resolve_expression(e, scopes, name_gen)?),

        Statement::If(cond, then_stmt, else_stmt) => Statement::If(
            resolve_expression(cond, scopes, name_gen)?,
            Box::new(resolve_statement(*then_stmt, scopes, name_gen)?),
            match else_stmt {
                Some(b) => Some(Box::new(resolve_statement(*b, scopes, name_gen)?)),
                None => None,
            },
        ),

        Statement::While(e, body, _) => Statement::While(
            resolve_expression(e, scopes, name_gen)?,
            Box::new(resolve_statement(*body, scopes, name_gen)?),
            None,
        ),

        Statement::DoWhile(body, e, _) => Statement::DoWhile(
            Box::new(resolve_statement(*body, scopes, name_gen)?),
            resolve_expression(e, scopes, name_gen)?,
            None,
        ),

        Statement::For(init, o_exp1, o_exp2, body, _) => {
            // for introduces its own scope (for the declaration!)
            scopes.push(HashMap::new());

            let init = resolve_for_init(init, scopes, name_gen)?;

            let exp1 = match o_exp1 {
                None => None,
                Some(e) => Some(resolve_expression(e, scopes, name_gen)?),
            };

            let exp2 = match o_exp2 {
                None => None,
                Some(e) => Some(resolve_expression(e, scopes, name_gen)?),
            };

            let body = Box::new(resolve_statement(*body, scopes, name_gen)?);

            // exited the loop; previous scope.
            scopes.pop();

            Statement::For(init, exp1, exp2, body, None)
        }

        Statement::Break(_) => Statement::Break(None),
        Statement::Continue(_) => Statement::Continue(None),

        // A compound statement `{ ... }` introduces a new scope.
        Statement::Compound(b) => Statement::Compound(resolve_block(
            b, scopes, name_gen, /*push_scope=*/ true,
        )?),

        // Labels/goto live in a separate namespace from variables.
        Statement::Goto(id) => Statement::Goto(id),

        Statement::Label(id, stmt) => {
            let stmt = resolve_statement(*stmt, scopes, name_gen)?;
            Statement::Label(id, Box::new(stmt))
        }

        Statement::Switch(exp, b_stmt, meta) => Statement::Switch(
            resolve_expression(exp, scopes, name_gen)?,
            Box::new(resolve_statement(*b_stmt, scopes, name_gen)?),
            meta,
        ),

        Statement::Case(i, lbl, b_stmt) => Statement::Case(
            i,
            lbl,
            Box::new(resolve_statement(*b_stmt, scopes, name_gen)?),
        ),

        Statement::Default(lbl, b_stmt) => Statement::Default(
            lbl,
            Box::new(resolve_statement(*b_stmt, scopes, name_gen)?),
        ),

        Statement::Null => Statement::Null,
    })
}

fn resolve_for_init(
    f: ForInit,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
) -> Result<ForInit, CompileError> {
    match f {
        ForInit::InitExpression(None) => Ok(ForInit::InitExpression(None)),
        ForInit::InitExpression(Some(exp)) => Ok(ForInit::InitExpression(Some(
            resolve_expression(exp, scopes, name_gen)?,
        ))),
        ForInit::InitDeclaration(dec) => Ok(ForInit::InitDeclaration(resolve_declaration(
            dec, scopes, name_gen,
        )?)),
    }
}

fn resolve_expression(
    e: Expression,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
) -> Result<Expression, CompileError> {
    Ok(match e {
        Expression::Constant(n) => Expression::Constant(n),

        Expression::Variable(id) => {
            let mapped = lookup(&id, scopes).ok_or_else(|| {
                CompileError::Semantic(format!("Use of undeclared variable {}", id.name))
            })?;
            Expression::Variable(mapped)
        }

        Expression::Unary(op, inner) => {
            let inner = resolve_expression(*inner, scopes, name_gen)?;
            Expression::Unary(op, Box::new(inner))
        }

        Expression::Binary(op, left, right) => {
            let left = resolve_expression(*left, scopes, name_gen)?;
            let right = resolve_expression(*right, scopes, name_gen)?;
            Expression::Binary(op, Box::new(left), Box::new(right))
        }

        Expression::Ternary(cond, then_e, else_e) => Expression::Ternary(
            Box::new(resolve_expression(*cond, scopes, name_gen)?),
            Box::new(resolve_expression(*then_e, scopes, name_gen)?),
            Box::new(resolve_expression(*else_e, scopes, name_gen)?),
        ),

        Expression::Assignment(op, left, right) => {
            let left = resolve_lvalue(*left, scopes)?;
            let right = resolve_expression(*right, scopes, name_gen)?;
            Expression::Assignment(op, Box::new(left), Box::new(right))
        }

        Expression::PreInc(inner) => Expression::PreInc(Box::new(resolve_lvalue(*inner, scopes)?)),
        Expression::PreDec(inner) => Expression::PreDec(Box::new(resolve_lvalue(*inner, scopes)?)),
        Expression::PostInc(inner) => {
            Expression::PostInc(Box::new(resolve_lvalue(*inner, scopes)?))
        }
        Expression::PostDec(inner) => {
            Expression::PostDec(Box::new(resolve_lvalue(*inner, scopes)?))
        }
    })
}

fn resolve_lvalue(e: Expression, scopes: &Scopes) -> Result<Expression, CompileError> {
    match e {
        Expression::Variable(id) => {
            let mapped = lookup(&id, scopes).ok_or_else(|| {
                CompileError::Semantic(format!("Use of undeclared variable {}", id.name))
            })?;
            Ok(Expression::Variable(mapped))
        }

        // Common “bad lvalues” at this project stage:
        Expression::Ternary(_, _, _) => Err(CompileError::Semantic(
            "Conditional expression is not assignable".to_string(),
        )),

        _ => Err(CompileError::Semantic(
            "Left-hand side of assignment must be an assignable expression".to_string(),
        )),
    }
}

/// Look up an identifier in the scope stack (innermost to outermost).
fn lookup(id: &Identifier, scopes: &Scopes) -> Option<Identifier> {
    for scope in scopes.iter().rev() {
        if let Some(mapped) = scope.get(id) {
            return Some(mapped.clone());
        }
    }
    None
}

/// Declare an identifier in the current (innermost) scope, generating a unique internal name.
/// Shadowing across scopes is allowed; duplicates *within the same scope* are rejected.
fn declare(
    id: Identifier,
    scopes: &mut Scopes,
    name_gen: &mut NameGen,
) -> Result<Identifier, CompileError> {
    let cur = scopes
        .last_mut()
        .expect("Scopes stack should always have at least one scope");

    if cur.contains_key(&id) {
        return Err(CompileError::Semantic(format!(
            "Duplicate variable {} declared in same scope.",
            id.name
        )));
    }

    let unique = name_gen.fresh_temp_ident();
    cur.insert(id, unique.clone());
    Ok(unique)
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
        Identifier {
            name: self.fresh_temp(),
        }
    }
}
