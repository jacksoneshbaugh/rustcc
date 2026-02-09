use crate::compile_error::CompileError;
use crate::parser::BinaryOperator::{Add, BitwiseAnd, BitwiseOr, Divide, LeftShift, Multiply, Remainder, RightShift, Subtract, Xor};
use crate::parser::{AssignOp, BinaryOperator, BlockItem, Declaration, Expression, Function, Identifier, Program, Statement};
use crate::tacky::ast::{TACKYFunction, TACKYInstruction, TACKYPlace, TACKYProgram, TACKYValue};
use crate::tacky::TACKYInstruction::{Binary, Copy, Jump, JumpIfZero, Label};
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

        // if (<cond>) <stmt>
        Statement::If(condition, then_branch, None) => {
            let (mut instrs, result) = tackify_expression(condition, allocator)?;
            let c = allocator.new_place();
            let end = allocator.new_label("if_end");

            instrs.push(Copy(result.clone(), c.clone()));
            instrs.push(JumpIfZero(Variable(c.0.clone()), end.clone()));
            let mut stmt_instrs = tackify_statement(*then_branch, allocator)?;
            instrs.append(&mut stmt_instrs);
            instrs.push(Label(end.clone()));

            Ok(instrs)
        },

        // if (<cond>) <stmt> else <stmt>
        Statement::If(condition, then_branch, Some(else_branch)) => {
            let (mut instrs, result) = tackify_expression(condition, allocator)?;
            let c = allocator.new_place();
            let else_lab = allocator.new_label("if_else");
            let end_lab = allocator.new_label("if_end");

            instrs.push(Copy(result.clone(), c.clone()));
            instrs.push(JumpIfZero(Variable(c.0.clone()), else_lab.clone()));
            let mut then_stmt_instrs = tackify_statement(*then_branch, allocator)?;
            instrs.append(&mut then_stmt_instrs);
            instrs.push(Jump(end_lab.clone()));

            instrs.push(Label(else_lab.clone()));
            let mut else_stmt_instrs = tackify_statement(*else_branch, allocator)?;
            instrs.append(&mut else_stmt_instrs);

            instrs.push(Label(end_lab.clone()));

            Ok(instrs)
        },

        Statement::Goto(identifier) => Ok(vec![Jump(identifier)]),
        Statement::Label(identifier, statement_box) => {
            let mut instrs: Vec<TACKYInstruction> = Vec::new();
            instrs.push(Label(identifier.clone()));
            let mut instrs_stmt = tackify_statement(*statement_box, allocator)?;
            instrs.append(&mut instrs_stmt);
            Ok(instrs)
        }

        Statement::Return(exp) => {
            let (mut instrs, val) = tackify_expression(exp, allocator)?;
            instrs.push(TACKYInstruction::Return(val));
            Ok(instrs)
        },
        Statement::Null => Ok(vec![])
    }
}

fn tackify_compound_assign(
    op: BinaryOperator,
    left: Expression,
    right: Expression,
    allocator: &mut TempAllocator
) -> Result<(Vec<TACKYInstruction>, TACKYValue), CompileError> {

    // tmp = (lhs <op> rhs);
    // lhs = tmp;
    // value is lhs

    let place = tackify_place(left)?;
    let lhs_value = TACKYValue::Variable(place.0.clone());
    let (mut instrs, rhs_value) = tackify_expression(right, allocator)?;

    let tmp = allocator.new_place();
    instrs.push(Binary(op, lhs_value, rhs_value, tmp.clone()));
    instrs.push(Copy(Variable(tmp.0.clone()), place.clone()));

    Ok((instrs, Variable(place.0)))
}

fn tackify_expression(
    parsed_expr: Expression,
    allocator: &mut TempAllocator,
) -> Result<(Vec<TACKYInstruction>, TACKYValue), CompileError> {
    match parsed_expr {
        Expression::Constant(i) => Ok((vec![], TACKYValue::Constant(i))),

        Expression::Variable(identifier) => Ok((vec![], TACKYValue::Variable(identifier))),

        Expression::Assignment(operator, left, right) => {
            match operator {

                // default assignment operator
                AssignOp::Assign => {
                    let place = tackify_place(*left)?; // lhs must be a place

                    // evaluate the rhs
                    let (mut instrs, rhs) = tackify_expression(*right, allocator)?;

                    // copy the value from the rhs to the lhs
                    instrs.push(Copy(rhs.clone(), place.clone()));
                    Ok((instrs, Variable(place.0)))
                },

                // compound assignment operators
                AssignOp::AddAssign => tackify_compound_assign(Add, *left, *right, allocator),
                AssignOp::SubAssign => tackify_compound_assign(Subtract, *left, *right, allocator),
                AssignOp::MulAssign => tackify_compound_assign(Multiply, *left, *right, allocator),
                AssignOp::DivAssign => tackify_compound_assign(Divide, *left, *right, allocator),
                AssignOp::ModAssign => tackify_compound_assign(Remainder, *left, *right, allocator),
                AssignOp::AndAssign => tackify_compound_assign(BitwiseAnd, *left, *right, allocator),
                AssignOp::OrAssign => tackify_compound_assign(BitwiseOr, *left, *right, allocator),
                AssignOp::XorAssign => tackify_compound_assign(Xor, *left, *right, allocator),
                AssignOp::ShlAssign => tackify_compound_assign(LeftShift, *left, *right, allocator),
                AssignOp::ShrAssign => tackify_compound_assign(RightShift, *left, *right, allocator)
            }
        },

        Expression::Ternary(condition, left, right) => {
            let (mut instrs, cond_val) = tackify_expression(*condition, allocator)?;

            let c = allocator.new_place();
            instrs.push(Copy(cond_val, c.clone()));

            let out = allocator.new_place();
            let else_lab = allocator.new_label("ternary_else");
            let end_lab  = allocator.new_label("ternary_end");

            instrs.push(JumpIfZero(Variable(c.0.clone()), else_lab.clone()));

            // then
            let (mut then_instrs, then_val) = tackify_expression(*left, allocator)?;
            instrs.append(&mut then_instrs);
            instrs.push(Copy(then_val, out.clone()));
            instrs.push(Jump(end_lab.clone()));

            // else
            instrs.push(Label(else_lab));
            let (mut else_instrs, else_val) = tackify_expression(*right, allocator)?;
            instrs.append(&mut else_instrs);
            instrs.push(Copy(else_val, out.clone()));

            instrs.push(Label(end_lab));

            Ok((instrs, Variable(out.0)))
        }

        // increment
        Expression::PreInc(inner) => {
            // ++x:
            //   tmp = x + 1
            //   x = tmp
            //   value is x (new value)

            let place = tackify_place(*inner)?; // must be assignable (variable for now)
            let lhs_val = TACKYValue::Variable(place.0.clone());

            let tmp = allocator.new_place();

            let mut instrs = Vec::new();
            instrs.push(TACKYInstruction::Binary(
                BinaryOperator::Add,
                lhs_val,
                TACKYValue::Constant(1),
                tmp.clone(),
            ));
            instrs.push(TACKYInstruction::Copy(
                TACKYValue::Variable(tmp.0.clone()),
                place.clone(),
            ));

            Ok((instrs, TACKYValue::Variable(place.0)))

        },

        Expression::PostInc(inner) => {
            // x++:
            //   old = x
            //   tmp = x + 1
            //   x = tmp
            //   value is old

            let place = tackify_place(*inner)?;
            let lhs_val = TACKYValue::Variable(place.0.clone());

            let old = allocator.new_place();
            let tmp = allocator.new_place();

            let mut instrs = Vec::new();

            // old = x
            instrs.push(Copy(lhs_val.clone(), old.clone()));

            // tmp = x + 1
            instrs.push(TACKYInstruction::Binary(
                BinaryOperator::Add,
                lhs_val,
                TACKYValue::Constant(1),
                tmp.clone(),
            ));

            // x = tmp
            instrs.push(TACKYInstruction::Copy(
                TACKYValue::Variable(tmp.0.clone()),
                place.clone()
            ));

            // value is old
            Ok((instrs, TACKYValue::Variable(old.0)))

        },

        // increment
        Expression::PreDec(inner) => {
            // --x:
            //   tmp = x - 1
            //   x = tmp
            //   value is x (new value)

            let place = tackify_place(*inner)?; // must be assignable (variable for now)
            let lhs_val = TACKYValue::Variable(place.0.clone());

            let tmp = allocator.new_place();

            let mut instrs = Vec::new();
            instrs.push(TACKYInstruction::Binary(
                BinaryOperator::Subtract,
                lhs_val,
                TACKYValue::Constant(1),
                tmp.clone(),
            ));
            instrs.push(TACKYInstruction::Copy(
                TACKYValue::Variable(tmp.0.clone()),
                place.clone(),
            ));

            Ok((instrs, TACKYValue::Variable(place.0)))

        },

        Expression::PostDec(inner) => {
            // x--:
            //   old = x
            //   tmp = x - 1
            //   x = tmp
            //   value is old

            let place = tackify_place(*inner)?;
            let lhs_val = TACKYValue::Variable(place.0.clone());

            let old = allocator.new_place();
            let tmp = allocator.new_place();

            let mut instrs = Vec::new();

            // old = x
            instrs.push(Copy(lhs_val.clone(), old.clone()));

            // tmp = x + 1
            instrs.push(TACKYInstruction::Binary(
                BinaryOperator::Subtract,
                lhs_val,
                TACKYValue::Constant(1),
                tmp.clone(),
            ));

            // x = tmp
            instrs.push(TACKYInstruction::Copy(
                TACKYValue::Variable(tmp.0.clone()),
                place.clone()
            ));

            // value is old
            Ok((instrs, TACKYValue::Variable(old.0)))

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