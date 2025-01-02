use indexmap::IndexMap;

// TODO comments

pub struct Body {
    statements: Vec<Statement>,
}

pub struct Identifier(String);

pub enum Statement {
    Const {
        binding: Identifier,
        expression: Expression,
    },
    Let {
        binding: Identifier,
        expression: Expression,
    },
    Return {
        expression: Expression,
    },
    Import {},
    Export {},
    ExportDefault {
        expression: Expression,
    },
}

pub enum Expression {
    Null,
    Undefined,
    // TODO: undefined?
    Boolean {
        value: bool,
    },
    Number {
        value: Number,
    },
    String {
        value: String,
    },
    Template {
        // TODO
    },
    Array {
        elements: Vec<ArrayEntry>,
    },
    Object {
        entries: Vec<ObjectEntry>,
    },

    If {
        first: Box<If>,
        else_ifs: Vec<If>,
        else_: Option<Box<Body>>,
    },

    Function {
        name: Identifier,
        arguments: Vec<Identifier>,
        body: Body,
    },

    UnaryOperator {
        operator: UnaryOperator,
        expression: Box<Expression>,
    },
    BinaryOperator {
        operator: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    TernaryConditional {
        condition: Box<Expression>,
        true_: Box<Expression>,
        false_: Box<Expression>,
    },
}

pub struct If {
    condition: Expression,
    body: Body,
}

pub enum UnaryOperator {
    BooleanNot,
    BitwiseNot,
    Negate,
}

pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Exponent,

    BooleanAnd,
    BooleanOr,

    Equal,
    NotEqual,

    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    Dot,
    DotNullish,
    NullishCoalesce,

    BitwiseAnd,
    BitwiseOr,
}

pub enum Number {}

pub enum ArrayEntry {
    Value { expression: Expression },
    Spread { expression: Expression },
}

pub enum ObjectEntry {
    Value { key: Expression, value: Expression },
    Spread { expression: Expression },
}
