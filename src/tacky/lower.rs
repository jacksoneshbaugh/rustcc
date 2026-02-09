use crate::compile_error::CompileError;
use crate::parser::{BinaryOperator, BlockItem, Declaration, Expression, Function, Identifier, Program, Statement};
use crate::tacky::ast::{TACKYFunction, TACKYInstruction, TACKYPlace, TACKYProgram, TACKYValue};
use crate::tacky::TACKYInstruction::Copy;
use crate::tacky::TACKYValue::Variable;

/**
rustcc | tacky/lower.rs
Contains logic to construct a TACKY AST from parsed C
Jackson Eshbaugh
Written while following "Writing a C Compiler" by Nora Sandler
*/

pub struct TempAllocator {
    temp: i32,
    label: i32,
}

impl TempAllocator {
    pub fn new() -> Self {
        Self { temp: 0, label: 0 }
    }

    pub fn new_place(&mut self) -> TACKYPlace {
        let id = Identifier { name: format!("t{}", self.temp) };
        self.temp += 1;
        TACKYPlace(id)
    }

    pub fn new_label(&mut self, prefix: &str) -> Identifier {
        let id = Identifier { name: format!("{}_{}", prefix, self.label) };
        self.label += 1;
        id
    }
}

pub fn tackify(ast: Program) -> Result<TACKYProgram, CompileError> {
    let mut allocator = TempAllocator::new();
    Ok(TACKYProgram {
        function: tackify_function(ast.function_definition, &mut allocator)?,
    })
}

fn tackify_function(
    parsed_fn: Function,
    allocator: &mut TempAllocator,
) -> Result<TACKYFunction, CompileError> {
    let mut instrs: Vec<TACKYInstruction> = Vec::new();

    for item in parsed_fn.body {
        let mut item_instrs = tackify_block_item(item, allocator)?;
        instrs.append(&mut item_instrs);
    }

    let needs_implicit_return = match instrs.last() {
        Some(TACKYInstruction::Return(_)) => false,
        _ => true,
    };

    if needs_implicit_return {
        instrs.push(TACKYInstruction::Return(TACKYValue::Constant(0)));
    }

    Ok(TACKYFunction {
        identifier: parsed_fn.identifier,
        instructions: instrs,
    })
}

fn tackify_block_item(parsed_block_item: BlockItem, allocator: &mut TempAllocator) -> Result<Vec<TACKYInstruction>, CompileError> {
    match parsed_block_item {
        BlockItem::Statement(stmt) => tackify_statement(stmt, allocator),
        BlockItem::Declaration(declaration) => {
            match declaration {
                Declaration::Declaration(_identifier, None) => Ok(vec![]),
                Declaration::Declaration(identifier, Some(expression)) => {
                    let place = TACKYPlace(identifier);
                    let (mut instrs, rhs) = tackify_expression(expression, allocator)?;
                    instrs.push(Copy(rhs.clone(), place.clone()));
                    Ok(instrs)
                }
            }
        }
    }
}

fn tackify_statement(parsed_stmt: Statement, allocator: &mut TempAllocator) -> Result<Vec<TACKYInstruction>, CompileError> {
    match parsed_stmt {
        Statement::Expression(e) => {
            let (instrs, _result) = tackify_expression(e, allocator)?;
            Ok(instrs)
        },
        Statement::Return(exp) => {
            let (mut instrs, val) = tackify_expression(exp, allocator)?;
            instrs.push(TACKYInstruction::Return(val));
            Ok(instrs)
        },
        Statement::Null => Ok(vec![])
    }
}

fn tackify_expression(
    parsed_expr: Expression,
    allocator: &mut TempAllocator,
) -> Result<(Vec<TACKYInstruction>, TACKYValue), CompileError> {
    match parsed_expr {
        Expression::Constant(i) => Ok((vec![], TACKYValue::Constant(i))),

        Expression::Variable(identifier) => Ok((vec![], TACKYValue::Variable(identifier))),

        Expression::Assignment(left, right) => {
            let place = tackify_place(*left)?;
            let (mut instrs, rhs) = tackify_expression(*right, allocator)?;
            instrs.push(Copy(rhs.clone(), place.clone()));
            Ok((instrs, Variable(place.0)))
        },

        Expression::Unary(op, inner) => {
            let (mut instrs, val) = tackify_expression(*inner, allocator)?;
            let dest_place = allocator.new_place();
            let result_val = TACKYValue::Variable(dest_place.0.clone());

            instrs.push(TACKYInstruction::Unary(op, val, dest_place));
            Ok((instrs, result_val))
        }

        // Short-circuit AND
        Expression::Binary(BinaryOperator::And, left, right) => {
            let (mut instrs, v1) = tackify_expression(*left, allocator)?;

            let result_place = allocator.new_place();
            let result_val = TACKYValue::Variable(result_place.0.clone());

            let false_label = allocator.new_label("false");
            let end_label = allocator.new_label("end");

            instrs.push(TACKYInstruction::JumpIfZero(v1.clone(), false_label.clone()));

            let (mut instrs2, v2) = tackify_expression(*right, allocator)?;
            instrs.append(&mut instrs2);

            instrs.push(TACKYInstruction::JumpIfZero(v2.clone(), false_label.clone()));

            instrs.push(TACKYInstruction::Copy(TACKYValue::Constant(1), result_place.clone()));
            instrs.push(TACKYInstruction::Jump(end_label.clone()));

            instrs.push(TACKYInstruction::Label(false_label));
            instrs.push(TACKYInstruction::Copy(TACKYValue::Constant(0), result_place));
            instrs.push(TACKYInstruction::Label(end_label));

            Ok((instrs, result_val))
        }

        // Short-circuit OR
        Expression::Binary(BinaryOperator::Or, left, right) => {
            let (mut instrs, v1) = tackify_expression(*left, allocator)?;

            let result_place = allocator.new_place();
            let result_val = TACKYValue::Variable(result_place.0.clone());

            let true_label = allocator.new_label("true");
            let end_label = allocator.new_label("end");

            instrs.push(TACKYInstruction::JumpIfNotZero(v1.clone(), true_label.clone()));

            let (mut instrs2, v2) = tackify_expression(*right, allocator)?;
            instrs.append(&mut instrs2);

            instrs.push(TACKYInstruction::JumpIfNotZero(v2.clone(), true_label.clone()));

            instrs.push(TACKYInstruction::Copy(TACKYValue::Constant(0), result_place.clone()));
            instrs.push(TACKYInstruction::Jump(end_label.clone()));

            instrs.push(TACKYInstruction::Label(true_label));
            instrs.push(TACKYInstruction::Copy(TACKYValue::Constant(1), result_place));
            instrs.push(TACKYInstruction::Label(end_label));

            Ok((instrs, result_val))
        }

        // Everything else
        Expression::Binary(op, left, right) => {
            let (mut instrs1, v1) = tackify_expression(*left, allocator)?;
            let (instrs2, v2) = tackify_expression(*right, allocator)?;

            let dest_place = allocator.new_place();
            let result_val = TACKYValue::Variable(dest_place.0.clone());

            instrs1.extend(instrs2);
            instrs1.push(TACKYInstruction::Binary(op, v1, v2, dest_place));

            Ok((instrs1, result_val))
        }
    }
}

fn tackify_place(expr: Expression) -> Result<TACKYPlace, CompileError> {
    match expr {
        Expression::Variable(id) => Ok(TACKYPlace(id)),
        _ => Err(CompileError::Syntax("Expected assignable place".to_string())),
    }
}