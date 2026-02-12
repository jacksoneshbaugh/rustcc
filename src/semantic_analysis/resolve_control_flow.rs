use crate::compile_error::CompileError;
use crate::parser::{Block, BlockItem, Function, Identifier, Program, Statement, SwitchMeta};

#[derive(Default)]
pub struct ControlFlowLabelGen {
    next: u64,
}

impl ControlFlowLabelGen {
    pub fn new() -> Self {
        ControlFlowLabelGen{ next: 0 }
    }

    pub fn fresh_loop_label(&mut self) -> String {
        let id = self.next;
        self.next += 1;
        format!("_loop{}", id)
    }

    pub fn fresh_loop_label_ident(&mut self) -> Identifier {
        Identifier {
            name: self.fresh_loop_label(),
        }
    }
}

pub fn resolve_control_flow(
    program: Program
) -> Result<Program, CompileError> {
    let mut loop_label_gen = ControlFlowLabelGen::new();

    let mut new_items = Vec::new();

    for item in program.function_definition.body.items {
        match item {
            BlockItem::Statement(stmt) => new_items.push(
                BlockItem::Statement(label_statement(stmt, None, None, false, &mut loop_label_gen)?)
            ),

            BlockItem::Declaration(declaration) => new_items.push(
                BlockItem::Declaration(declaration)
            )
        }
    }

    Ok(Program {
        function_definition: Function {
            identifier: program.function_definition.identifier,
            body: Block { items: new_items }
        }
    })
}

fn label_statement(
    statement: Statement,
    break_target: Option<Identifier>,
    continue_target: Option<Identifier>,
    in_switch: bool,
    label_gen: &mut ControlFlowLabelGen,
) -> Result<Statement, CompileError> {

    match statement {
        Statement::Break(_) => match break_target {
            Some(lbl) => Ok(Statement::Break(Some(lbl))),
            None => Err(CompileError::Semantic("Break statement outside of loop or switch".into())),
        },

        Statement::Continue(_) => match continue_target {
            Some(lbl) => Ok(Statement::Continue(Some(lbl))),
            None => Err(CompileError::Semantic("Continue statement outside of loop".into())),
        },

        Statement::While(condition, body, _) => {
            // We're in a loop, make a new label!
            let new_label = label_gen.fresh_loop_label_ident();
            Ok(Statement::While(
                condition,
                Box::new(label_statement(
                    *body, Some(new_label.clone()), Some(new_label.clone()), in_switch, label_gen)?),
                Some(new_label),
            ))
        }

        Statement::DoWhile(body, condition, _) => {
            // We're in a loop, make a new label!
            let new_label = label_gen.fresh_loop_label_ident();

            Ok(Statement::DoWhile(
                Box::new(label_statement(
                    *body, Some(new_label.clone()), Some(new_label.clone()), in_switch, label_gen)?),
                condition,
                Some(new_label),
            ))
        }

        Statement::For(init, o_exp1, o_exp2, body, _) => {
            // We're in a loop, make a new label!
            let new_label = label_gen.fresh_loop_label_ident();

            Ok(Statement::For(
                init,
                o_exp1,
                o_exp2,
                Box::new(label_statement(
                    *body, Some(new_label.clone()), Some(new_label.clone()), in_switch, label_gen)?),
                Some(new_label),
            ))
        }

        Statement::Label(name, stmt) => Ok(
            Statement::Label(
                name,
                Box::new(label_statement(*stmt, break_target, continue_target, in_switch, label_gen)?),
            )
        ),

        Statement::Return(_) | Statement::Expression(_) | Statement::Goto(_)
        | Statement::Null => Ok(statement),

        Statement::Compound(block) => {
            let mut new_items = vec![];

            for item in block.items {
                match item {
                    BlockItem::Declaration(d) => {
                        new_items.push(BlockItem::Declaration(d));
                    }
                    BlockItem::Statement(statement) => {
                        new_items.push(
                            BlockItem::Statement(
                                label_statement(statement, break_target.clone(), continue_target.clone(), in_switch, label_gen)?));
                    }
                }
            }

            Ok(Statement::Compound(Block { items: new_items }))
        }

        Statement::If(_cond, then_do, None) =>
            Ok(Statement::If(
                _cond,
                Box::new(label_statement(*then_do, break_target, continue_target, in_switch, label_gen)?),
                None
            )),

        Statement::If(_cond, then_do, Some(else_do)) =>
            Ok(Statement::If(
                _cond,
                Box::new(label_statement(*then_do, break_target.clone(), continue_target.clone(), in_switch, label_gen)?),
                Some(Box::new(label_statement(*else_do, break_target, continue_target, in_switch, label_gen)?))
            )),
        Statement::Switch(exp, b_stmt, _) => {
            let break_label = label_gen.fresh_loop_label_ident();

            let (new_body, cases, default) = label_switch_body(
                *b_stmt,
                Some(break_label.clone()),
                continue_target,
                label_gen,
            )?;

            let meta = SwitchMeta {
                break_label: break_label.clone(),
                cases,
                default,
            };

            Ok(Statement::Switch(exp, Box::new(new_body), Some(meta)))
        }

        Statement::Case(i, ident, b_stmt) => {
            if !in_switch {
                Err(CompileError::Semantic("Case statement outside of switch statement".into()))
            } else {
                Ok(Statement::Case(
                    i,
                    ident, // helper will fill None if needed
                    Box::new(label_statement(*b_stmt, break_target, continue_target, in_switch, label_gen)?),
                ))
            }
        }

        Statement::Default(ident, b_stmt) => {
            if !in_switch {
                Err(CompileError::Semantic("Default statement outside of switch statement".into()))
            } else {
                Ok(Statement::Default(
                    ident, // helper will fill None if needed
                    Box::new(label_statement(*b_stmt, break_target, continue_target, in_switch, label_gen)?),
                ))
            }
        }
    }

}

fn label_switch_body(
    stmt: Statement,
    break_target: Option<Identifier>,
    continue_target: Option<Identifier>,
    label_gen: &mut ControlFlowLabelGen,
) -> Result<(Statement, Vec<(i32, Identifier)>, Option<Identifier>), CompileError> {
    let mut cases: Vec<(i32, Identifier)> = vec![];
    let mut default: Option<Identifier> = None;

    fn go(
        s: Statement,
        break_target: Option<Identifier>,
        continue_target: Option<Identifier>,
        label_gen: &mut ControlFlowLabelGen,
        cases: &mut Vec<(i32, Identifier)>,
        default: &mut Option<Identifier>,
    ) -> Result<Statement, CompileError> {
        Ok(match s {
            Statement::Case(k, lab_opt, inner) => {
                let lab = lab_opt.unwrap_or_else(|| label_gen.fresh_loop_label_ident());
                if cases.iter().any(|(seen_k, _)| *seen_k == k) {
                    return Err(CompileError::Semantic(format!(
                        "Duplicate case label {} in switch",
                        k
                    )));
                }
                cases.push((k, lab.clone()));
                let inner2 = go(*inner, break_target, continue_target, label_gen, cases, default)?;
                Statement::Case(k, Some(lab), Box::new(inner2))
            }

            Statement::Default(lab_opt, inner) => {
                let lab = lab_opt.unwrap_or_else(|| label_gen.fresh_loop_label_ident());
                if default.is_some() {
                    return Err(CompileError::Semantic(
                        "Duplicate default label in switch".into()
                    ));
                }
                *default = Some(lab.clone());
                let inner2 = go(*inner, break_target, continue_target, label_gen, cases, default)?;
                Statement::Default(Some(lab), Box::new(inner2))
            }

            // recurse through blocks and structured statements
            Statement::Compound(block) => {
                let mut new_items = vec![];
                for it in block.items {
                    new_items.push(match it {
                        BlockItem::Declaration(d) => BlockItem::Declaration(d),
                        BlockItem::Statement(st) => BlockItem::Statement(
                            go(st, break_target.clone(), continue_target.clone(), label_gen, cases, default)?
                        ),
                    });
                }
                Statement::Compound(Block { items: new_items })
            }

            Statement::If(c, t, e) => {
                let t2 = go(*t, break_target.clone(), continue_target.clone(), label_gen, cases, default)?;
                let e2 = match e {
                    Some(b) => Some(Box::new(go(*b, break_target, continue_target, label_gen, cases, default)?)),
                    None => None,
                };
                Statement::If(c, Box::new(t2), e2)
            }

            Statement::While(c, b, lbl) => {
                let b2 = go(*b, break_target, continue_target, label_gen, cases, default)?;
                Statement::While(c, Box::new(b2), lbl)
            }

            Statement::DoWhile(b, c, lbl) => {
                let b2 = go(*b, break_target, continue_target, label_gen, cases, default)?;
                Statement::DoWhile(Box::new(b2), c, lbl)
            }

            Statement::For(i, c, p, b, lbl) => {
                let b2 = go(*b, break_target, continue_target, label_gen, cases, default)?;
                Statement::For(i, c, p, Box::new(b2), lbl)
            }

            Statement::Label(name, inner) => {
                let inner2 = go(*inner, break_target, continue_target, label_gen, cases, default)?;
                Statement::Label(name, Box::new(inner2))
            }

            // everything else: keep as-is (break/continue already handled by label_statement)
            other => other,
        })
    }

    // First, run your normal labeling with in_switch=true so break; becomes Break(Some(...))
    let labeled = label_statement(stmt, break_target.clone(), continue_target.clone(), true, label_gen)?;

    // Then, assign case/default labels and collect meta
    let with_cases = go(labeled, break_target, continue_target, label_gen, &mut cases, &mut default)?;

    Ok((with_cases, cases, default))
}