use crate::types::*;
use nom::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::sequence::*;
use nom::bytes::complete::*;
use nom::multi::*;
use nom::branch::*;
use nom::error::ParseError;
use nom::character::is_alphanumeric;

macro_rules! with_ws {
    ($parser: expr) => {
        nom::sequence::delimited(multispace0, $parser, multispace0)
    };
}

pub fn parse_program<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Program, E> {
    all_consuming(program)(i)
}

fn program<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Program, E> {
    let (i, decls) = many0(declaration)(i)?;
    let (i, stmts) = many0(statement)(i)?;
    Ok((i, Program { decls, stmts }))
}

fn statement<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    alt((
        map(tag(";"), |_| Stmt::Nop),
        assignment,
        assignment_read,
        assignment_write,
        block,
        if_else_stmt,
        if_stmt,
        while_stmt,
    ))(i)
}

fn condition<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Cond, E> {
    alt((
        bbinop_cond,
        map(with_ws!(tag("true")), |_| Cond::True),
        map(with_ws!(tag("false")), |_| Cond::False),
        map(
            delimited(with_ws!(tag("(")), condition, with_ws!(tag(")"))),
            |cond| Cond::Nop(Box::new(cond)),
        ),
        comparison,
        bunop_cond,
    ))(i)
}

fn condition_non_rec<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Cond, E> {
    alt((
        map(with_ws!(tag("true")), |_| Cond::True),
        map(with_ws!(tag("false")), |_| Cond::False),
        map(
            delimited(with_ws!(tag("(")), condition, with_ws!(tag(")"))),
            |cond| Cond::Nop(Box::new(cond)),
        ),
        comparison,
        bunop_cond,
    ))(i)
}

fn bbinop_cond<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Cond, E> {
    let (i, left) = condition_non_rec(i)?;
    let (i, op) = bbinop(i)?;
    let (i, right) = condition(i)?;
    Ok((
        i,
        Cond::BBinop {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    ))
}

fn compop<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Compop, E> {
    alt((
        map(with_ws!(tag("==")), |_| Compop::Eq),
        map(with_ws!(tag("!=")), |_| Compop::Neq),
        map(with_ws!(tag("<=")), |_| Compop::Lte),
        map(with_ws!(tag(">=")), |_| Compop::Gte),
        map(with_ws!(tag("<")), |_| Compop::Lt),
        map(with_ws!(tag(">")), |_| Compop::Gt),
    ))(i)
}

fn comparison<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Cond, E> {
    let (i, left) = with_ws!(expression)(i)?;
    let (i, op) = compop(i)?;
    let (i, right) = with_ws!(expression)(i)?;
    Ok((i, Cond::Comp { op, left, right }))
}

fn bunop_cond<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Cond, E> {
    let (i, op) = bunop(i)?;
    let (i, cond) = with_ws!(condition)(i)?;
    Ok((
        i,
        Cond::Bunop {
            op,
            cond: Box::new(cond),
        },
    ))
}

fn bunop<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Bunop, E> {
    map(with_ws!(tag("!")), |_| Bunop::Neg)(i)
}

fn bbinop<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, BBinop, E> {
    alt((
        map(with_ws!(tag("&&")), |_| BBinop::And),
        map(with_ws!(tag("||")), |_| BBinop::Or),
    ))(i)
}

fn if_stmt<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    let (i, _) = with_ws!(tag("if"))(i)?;
    let (i, cond) = delimited(with_ws!(tag("(")), with_ws!(condition), with_ws!(tag(")")))(i)?;
    let (i, stmt) = statement(i)?;
    Ok((
        i,
        Stmt::If {
            cond,
            stmt: Box::new(stmt),
        },
    ))
}

fn if_else_stmt<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    let (i, _) = with_ws!(tag("if"))(i)?;
    let (i, cond) = delimited(with_ws!(tag("(")), with_ws!(condition), with_ws!(tag(")")))(i)?;
    let (i, (stmt, else_stmt)) = separated_pair(statement, with_ws!(tag("else")), statement)(i)?;
    Ok((
        i,
        Stmt::IfElse {
            cond,
            stmt: Box::new(stmt),
            else_stmt: Box::new(else_stmt),
        },
    ))
}

fn while_stmt<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    let (i, _) = with_ws!(tag("while"))(i)?;
    let (i, cond) = delimited(tag("("), with_ws!(condition), tag(")"))(i)?;
    let (i, stmt) = statement(i)?;
    Ok((
        i,
        Stmt::While {
            cond,
            stmt: Box::new(stmt),
        },
    ))
}

fn assignment_write<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    let (i, _) = with_ws!(tag("write("))(i)?;
    let (i, expr) = terminated(expression, tag(")"))(i)?;
    let (i, _) = with_ws!(tag(";"))(i)?;
    Ok((i, Stmt::IoWrite(expr)))
}

fn assignment_read<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    let (i, n) = name(i)?;
    let (i, _) = with_ws!(tag("="))(i)?;
    let (i, _) = terminated(tag("read()"), with_ws!(tag(";")))(i)?;
    Ok((i, Stmt::IoRead(n)))
}

fn assignment<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    terminated(
        map(
            separated_pair(with_ws!(name), tag("="), with_ws!(expression)),
            |(name, expr)| Stmt::Assignment { name, expr },
        ),
        with_ws!(tag(";")),
    )(i)
}

fn expression<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Expr, E> {
    alt((
        binop_expr,
        map(with_ws!(name), |n| Expr::Name(n)),
        map(
            delimited(with_ws!(tag("(")), expression, with_ws!(tag(")"))),
            |e| Expr::Expr(Box::new(e)),
        ),
        map(pair(unop, expression), |(op, expr)| Expr::Unop {
            op,
            expr: Box::new(expr),
        }),
        map(with_ws!(u64), |n| Expr::Number(n)),
    ))(i)
}

fn expression_non_rec<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Expr, E> {
    alt((
        map(with_ws!(name), |n| Expr::Name(n)),
        delimited(with_ws!(tag("(")), expression, with_ws!(tag(")"))),
        map(pair(unop, expression), |(op, expr)| Expr::Unop {
            op,
            expr: Box::new(expr),
        }),
        map(with_ws!(u64), |n| Expr::Number(n)),
    ))(i)
}

fn binop_expr<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Expr, E> {
    let (i, left) = expression_non_rec(i)?;
    let (i, op) = binop(i)?;
    let (i, right) = expression(i)?;
    Ok((
        i,
        Expr::Binop {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    ))
}

fn binop<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Binop, E> {
    with_ws!(alt((
        map(with_ws!(tag("-")), |_| Binop::Minus),
        map(with_ws!(tag("+")), |_| Binop::Plus),
        map(with_ws!(tag("*")), |_| Binop::Multilply),
        map(with_ws!(tag("/")), |_| Binop::Divide),
        map(with_ws!(tag("%")), |_| Binop::Modulo),
    )))(i)
}

fn unop<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Unop, E> {
    map(with_ws!(tag("-")), |_| Unop::Minus)(i)
}

fn block<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Stmt, E> {
    map(delimited(
            with_ws!(tag("{")), many0(statement), with_ws!(tag("}"))
        ), |stmt_lst| Stmt::Block(stmt_lst) )(i)
}

// decl ::= type name (, name)*;
fn declaration<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, Decl, E> {
    map(preceded(
        with_ws!(tag("int")),
        terminated(
            separated_list1(with_ws!(tag(",")), name),
            with_ws!(tag(";"))
        )
    ), |idents| Decl { idents } )(i)
}

// name ::= letter | ( letter | _ ) ( letter | _ | digit ) ( letter | _ | digit )*
fn name<'a, E: ParseError<&'a str>>(i: &'a str)
        -> IResult<&'a str, String, E> {

    // Variables are not allowed to start with a digit
    not(peek(take_till1(|x| !(x as char).is_numeric())))(i)?;

    let (i, name) = map(take_till1(
        |i| !(is_alphanumeric(i as u8) || i == '$' || i == '_')
    ), |s| String::from(s))(i)?;

    // the singe underscore is reserved
    if name.eq("_") {
        return fail(i);
    }
    Ok((i, name))
}
