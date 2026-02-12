#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

pub struct Program {
    pub function_definition: Function,
}

pub struct Function {
    pub identifier: Identifier,
    pub body: Block,
}

pub struct Block {
    pub items: Vec<BlockItem>,
}

pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

pub struct SwitchMeta {
    pub break_label: Identifier,
    pub cases: Vec<(i32, Identifier)>,
    pub default: Option<Identifier>,
}

pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Compound(Block),
    Goto(Identifier),
    Label(Identifier, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
    While(Expression, Box<Statement>, Option<Identifier>),
    DoWhile(Box<Statement>, Expression, Option<Identifier>),
    For(ForInit, Option<Expression>, Option<Expression>, Box<Statement>, Option<Identifier>),
    Switch(Expression, Box<Statement>, Option<SwitchMeta>),
    Case(i32, Option<Identifier>, Box<Statement>),
    Default(Option<Identifier>, Box<Statement>),
    Null
}

pub enum ForInit {
    InitExpression(Option<Expression>),
    InitDeclaration(Declaration),
}

pub enum Declaration {
    Declaration(Identifier, Option<Expression>),
}

#[derive(Clone, Copy, Debug)]
pub enum AssignOp {
    Assign,        // =
    AddAssign,     // +=
    SubAssign,     // -=
    MulAssign,     // *=
    DivAssign,     // /=
    ModAssign,     // %=
    AndAssign,     // &=
    OrAssign,      // |=
    XorAssign,     // ^=
    ShlAssign,     // <<=
    ShrAssign,     // >>=
}

pub enum Expression {
    Constant(i32),
    Variable(Identifier),

    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

    // assignment family
    Assignment(AssignOp, Box<Expression>, Box<Expression>),

    PreInc(Box<Expression>),
    PreDec(Box<Expression>),
    PostInc(Box<Expression>),
    PostDec(Box<Expression>),
}

pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

pub enum BinaryOperator {
    Add, Subtract, Multiply, Divide, Remainder,
    BitwiseAnd, BitwiseOr, Xor, LeftShift, RightShift,
    And, Or, Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual,
}