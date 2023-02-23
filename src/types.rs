use std::fmt::Debug;

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Decl {
    pub idents: Vec<String>,
}

#[derive(Debug)]
pub enum Stmt {
    Nop,
    Block(Vec<Stmt>),
    Assignment {
        name: String,
        expr: Expr,
    },
    IoRead(String),
    IoWrite(Expr),
    If {
        cond: Cond,
        stmt: Box<Stmt>,
    },
    IfElse {
        cond: Cond,
        stmt: Box<Stmt>,
        else_stmt: Box<Stmt>,
    },
    While {
        cond: Cond,
        stmt: Box<Stmt>,
    },
}

#[derive(Debug)]
pub enum Expr {
    Number(u64),
    Name(String),
    Expr(Box<Expr>),
    Unop {
        op: Unop,
        expr: Box<Expr>,
    },
    Binop {
        op: Binop,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Cond {
    True,
    False,
    Nop(Box<Cond>),
    Comp {
        op: Compop,
        left: Expr,
        right: Expr,
    },
    Bunop {
        op: Bunop,
        cond: Box<Cond>,
    },
    BBinop {
        op: BBinop,
        left: Box<Cond>,
        right: Box<Cond>,
    },
}

#[derive(Debug)]
pub enum Bunop {
    Neg,
}

#[derive(Debug)]
pub enum BBinop {
    And,
    Or,
}

#[derive(Debug)]
pub enum Compop {
    Eq,
    Neq,
    Lte,
    Gte,
    Lt,
    Gt,
}

#[derive(Debug)]
pub enum Unop {
    Minus,
}

#[derive(Debug)]
pub enum Binop {
    Minus,
    Plus,
    Multilply,
    Divide,
    Modulo,
}

