use crate::types::*;

use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{u32, alpha1, alphanumeric0, multispace0 as ws},
    combinator::{consumed, map},
    multi::{many0, separated_list1},
    sequence::{delimited, terminated, tuple, pair, separated_pair},
};

macro_rules! with_ws {
    ($parser: expr) => {
        nom::sequence::delimited(ws, $parser, ws)
    }
}

pub fn program(i: &str) -> IResult<&str, Program> {
    let (i, decls) = many0(declaration)(i)?;
    let (i, stmts) = many0(statement)(i)?;
    Ok((
        i,
        Program {
            decls,
            stmts,
        })
    )
}

fn statement(i: &str) -> IResult<&str, Stmt> {
    alt((
        map(tag(";"), |_| Stmt::Nop),
        block,
        assignment,
        assignment_read,
        assignment_write,
        if_else_stmt,
        if_stmt,
        while_stmt,
    ))(i)
}

fn condition(i: &str) -> IResult<&str, Cond> {
    alt((
        bbinop_cond,
        map(with_ws!(tag("true")), |_| Cond::True),
        map(with_ws!(tag("false")), |_| Cond::False),
        map(delimited(
            with_ws!(tag("(")),
            condition,
            with_ws!(tag(")")),
        ),
        |cond| Cond::Nop(Box::new(cond))),
        comparison,
        bunop_cond,
    ))(i)
}

fn condition_non_rec(i: &str) -> IResult<&str, Cond> {
    alt((
        map(with_ws!(tag("true")), |_| Cond::True),
        map(with_ws!(tag("false")), |_| Cond::False),
        map(delimited(
            with_ws!(tag("(")),
            condition,
            with_ws!(tag(")")),
        ),
        |cond| Cond::Nop(Box::new(cond))),
        comparison,
        bunop_cond,
    ))(i)
}

fn bbinop_cond(i: &str) -> IResult<&str, Cond> {
    let (i, left) = condition_non_rec(i)?;
    let (i, op) = bbinop(i)?;
    let (i, right) = condition(i)?;
    Ok((i, Cond::BBinop {
        op,
        left: Box::new(left),
        right: Box::new(right),
    }))
}

fn compop(i: &str) -> IResult<&str, Compop> {
    alt((
        map(with_ws!(tag("==")), |_| Compop::Eq),
        map(with_ws!(tag("!=")), |_| Compop::Neq),
        map(with_ws!(tag("<=")), |_| Compop::Lte),
        map(with_ws!(tag(">=")), |_| Compop::Gte),
        map(with_ws!(tag("<")), |_| Compop::Lt),
        map(with_ws!(tag(">")), |_| Compop::Gt),
    ))(i)
}

fn comparison(i: &str) -> IResult<&str, Cond> {
    let (i, left) = with_ws!(expression)(i)?;
    let (i, op) = compop(i)?;
    let (i, right) = with_ws!(expression)(i)?;
    Ok((i, Cond::Comp {
        op,
        left,
        right,
    }))
}

fn bunop_cond(i: &str) -> IResult<&str, Cond> {
    let (i, op) = bunop(i)?;
    let (i, cond) = with_ws!(condition)(i)?;
    Ok((i, Cond::Bunop{
        op,
        cond: Box::new(cond),
    }))
}

fn bunop(i: &str) -> IResult<&str, Bunop> {
    map(with_ws!(tag("!")), |_| Bunop::Neg)(i)
}


fn bbinop(i: &str) -> IResult<&str, BBinop> {
    alt((
        map(with_ws!(tag("&&")), |_| BBinop::And),
        map(with_ws!(tag("||")), |_| BBinop::Or),
    ))(i)
}

fn if_stmt(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = with_ws!(tag("if"))(i)?;
    let (i, cond) = delimited(with_ws!(tag("(")), with_ws!(condition), with_ws!(tag(")")))(i)?;
    let (i, stmt) = statement(i)?;
    Ok((i, Stmt::If {
        cond,
        stmt: Box::new(stmt),
    }))
}

fn if_else_stmt(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = with_ws!(tag("if"))(i)?;
    let (i, cond) = delimited(with_ws!(tag("(")), with_ws!(condition), with_ws!(tag(")")))(i)?;
    let (i, (stmt, else_stmt)) = separated_pair(
        statement,
        with_ws!(tag("else")),
        statement,
    )(i)?;
    Ok((i, Stmt::IfElse {
        cond,
        stmt: Box::new(stmt),
        else_stmt: Box::new(else_stmt),
    }))
}

fn while_stmt(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = with_ws!(tag("while"))(i)?;
    let (i, cond) = delimited(tag("("), with_ws!(condition), tag(")"))(i)?;
    let (i, stmt) = statement(i)?;
    Ok((i, Stmt::While {
        cond,
        stmt: Box::new(stmt),
    }))
}

fn assignment_write(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = with_ws!(
        tag("write("))(i)?;
    let (i, expr) = terminated(expression, tag(")"))(i)?;
    let (i, _) = with_ws!(tag(";"))(i)?;
    Ok((i, Stmt::IoWrite(expr)))
}

fn assignment_read(i: &str) -> IResult<&str, Stmt> {
    let (i, n) = name(i)?;
    let (i, _) = with_ws!(tag("="))(i)?;
    let (i, _) = terminated(
        tag("read()"),
        with_ws!(tag(";"))
    )(i)?;
    Ok((i, Stmt::IoRead(n)))
}

fn assignment(i: &str) -> IResult<&str, Stmt> {
    terminated(
        map(
            separated_pair(
                with_ws!(name),
                tag("="),
                with_ws!(expression),
            ),
            |(name, expr)| Stmt::Assignment {
                name,
                expr,
            }
        ),
        with_ws!(tag(";"))
    )(i)
}

fn expression(i: &str) -> IResult<&str, Expr> {
    alt((
        binop_expr,
        map(with_ws!(name), |n| Expr::Name(n)),
        map(
            delimited(with_ws!(tag("(")), expression, with_ws!(tag(")"))),
            |e| Expr::Expr(Box::new(e))
        ),
        map(pair(unop, expression), |(op, expr)| Expr::Unop {
            op,
            expr: Box::new(expr),
        }),
        map(with_ws!(u32), |n| Expr::Number(n)),
    ))(i)
}

fn expression_non_rec(i: &str) -> IResult<&str, Expr> {
    alt((
        map(with_ws!(name), |n| Expr::Name(n)),
        delimited(with_ws!(tag("(")), expression, with_ws!(tag(")"))),
        map(pair(unop, expression), |(op, expr)| Expr::Unop {
            op,
            expr: Box::new(expr),
        }),
        map(with_ws!(u32), |n| Expr::Number(n)),
    ))(i)
}


fn binop_expr(i: &str) -> IResult<&str, Expr> {
    let (i, left) = expression_non_rec(i)?;
    let (i, op) = binop(i)?;
    let (i, right) = expression(i)?;
    Ok((i, Expr::Binop {
        op,
        left: Box::new(left),
        right: Box::new(right),
    }))
}

fn binop(i: &str) -> IResult<&str, Binop> {
    with_ws!(alt((
        map(with_ws!(tag("-")), |_| Binop::Minus),
        map(with_ws!(tag("+")), |_| Binop::Plus),
        map(with_ws!(tag("*")), |_| Binop::Multilply),
        map(with_ws!(tag("/")), |_| Binop::Divide),
        map(with_ws!(tag("%")), |_| Binop::Modulo),
    )))(i)
}

fn unop(i: &str) -> IResult<&str, Unop> {
    map(with_ws!(tag("-")), |_| Unop::Minus)(i)
}

fn block(i: &str) -> IResult<&str, Stmt> {
    map(
        delimited(with_ws!(tag("{")), many0(statement), with_ws!(tag("}"))),
        |stmt_lst| Stmt::Block(stmt_lst)
    )(i)
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

