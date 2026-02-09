#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

pub struct Program {
    pub function_definition: Function,
}

pub struct Function {
    pub identifier: Identifier,
    pub body: Vec<BlockItem>,
}

pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Goto(Identifier),
    Label(Identifier, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Null
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