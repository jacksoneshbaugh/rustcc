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
    Null
}

pub enum Declaration {
    Declaration(Identifier, Option<Expression>),
}

pub enum Expression {
    Constant(i32),
    Variable(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
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