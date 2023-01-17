use nom::{
    IResult,
    bytes::complete::tag,
    character::complete::multispace0 as wh,
    multi::separated_list1,
    combinator::consumed,
    sequence::delimited,
    sequence::tuple,
    character::complete::alpha1,
    character::complete::alphanumeric0,
    multi::many0,
    sequence::terminated,
    combinator::map,
    branch::alt,
};

use std::fmt::Debug;

#[derive(Debug)]
struct Program {
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
struct Decl {
    idents: Vec<String>,
}

#[derive(Debug)]
enum Stmt {
    Nop,
    Block (Vec<Stmt>),
    Assignment {
        name: String,
        expr: Expr,
    },
    IoRead (String),
    IoExpr (Expr),
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
enum Expr {
    Number (u32),
    Name (String),
    Expr (Box<Expr>),
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
enum Cond {
    True,
    False,
    Nop (Box<Cond>),
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
enum Bunop {
    Neg,
}

#[derive(Debug)]
enum BBinop {
    And,
    Or,
}

#[derive(Debug)]
enum Compop {
    Eq,
    Neq,
    Lte,
    Gte,
    Lt,
    Gt,
}

#[derive(Debug)]
enum Unop {
    Minus,
}

#[derive(Debug)]
enum Binop {
    Minus,
    Plus,
    Multilply,
    Divide,
    Modulo,
}

fn main() {
    let i = "
        int x,       r;
        int n              ;
        int in, temp, digits    , digit, sum, power, c;
    ";

    let (i, ast) = program(i).unwrap();
    println!("i : '{}'\n{:?}", i, ast);
}

fn program(i: &str) -> IResult<&str, Program> {
    let (i, decls) = many0(declaration)(i)?;
    Ok((
        i,
        Program {
            decls,
            stmts: Vec::new()
        })
    )
}

fn statement(i: &str) -> IResult<&str, Stmt> {
    alt((
        map(tag(";"), |_| Stmt::Nop),
        block,
        terminated(
            map(),
            with_ws!(tag(";"))
        )
    ))(i)
}

fn block(i: &str) -> IResult<&str, Stmt> {
    map(
        delimited(tag("{"), many0(statement), tag("}")),
        |stmt_lst| Stmt::Block(stmt_lst)
    )(i)
}

macro_rules! with_ws {
    ($parser: expr) => {
        nom::sequence::delimited(wh, $parser, wh)
    }
}

fn declaration(i: &str) -> IResult<&str, Decl> {
    let (i, _) = with_ws!(tag("int"))(i)?;
    let (i, idents) = separated_list1(
        with_ws!(
            tag(",")
        ),
        name
    )(i)?;
    let (i, _) = with_ws!(tag(";"))(i)?;
    Ok((i, Decl {idents,}))
}

fn name(i: &str) -> IResult<&str, String> {
    let (i, (name, _)) = consumed(tuple(
        (alpha1, alphanumeric0)
    ))(i)?;
    Ok((i, name.to_owned()))
}

