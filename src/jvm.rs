use crate::types::*;
use std::collections::HashMap;

pub struct Instruction {
    pub pop: u8,
    pub push: u8,
    pub id: InstID,
}

#[allow(non_camel_case_types)]
pub enum InstID {
    // int opreations
    NEG,
    ADD,
    SUB,
    MUL,
    DIV,

    // boolean operations
    MOD,
    NOT,
    AND,
    OR,

    // comparison operations
    LESS,
    LEQ,
    EQ,
    NEQ,
    GREATER, // NICHT AUS DER VORLESUNG
    GEQ,     // NICHT AUS DER VORLESUNG

    // constants
    CONST(u64),
    TRUE,
    FALSE,

    // memory operations
    LOAD(usize),
    STORE(usize),
    ALLOC(usize),

    // control flow
    JUMP(String),
    FJUMP(String),

    // io operations
    READ,
    WRITE,

    HALT,
    JUMP_TARGET(String),
}

pub struct JVMProgram {
    pub instructions: Vec<Instruction>,
    label_count: usize,
}

impl JVMProgram {
    fn push(&mut self, pop: u8, push: u8, id: InstID) {
        self.instructions.push(Instruction { pop, push, id });
    }
}

trait GenerateJvm {
    fn to_jvm(&self, prog: &mut JVMProgram, vars: &HashMap<String, usize>);
}

// convert the syntax tree into a list of jvm instructions
pub fn ast2jvm(ast: &Program) -> JVMProgram {
    let mut prog = JVMProgram {
        instructions: Vec::new(),
        label_count: 0,
    };

    let mut vars = HashMap::<String, usize>::new();
    let mut offset = 0;
    for decl in &ast.decls {
        for ident in &decl.idents {
            offset += 1;
            vars.insert(ident.to_owned(), offset);
        }
        prog.push(0, 0, InstID::ALLOC(decl.idents.len()));
    }

    for stmt in &ast.stmts {
        stmt.to_jvm(&mut prog, &vars);
    }

    prog.push(0, 0, InstID::HALT);
    prog
}

impl GenerateJvm for Stmt {
    fn to_jvm(&self, prog: &mut JVMProgram, vars: &HashMap<String, usize>) {
        match self {
            Stmt::Nop => (),
            Stmt::Block(v) => {
                for s in v {
                    s.to_jvm(prog, vars);
                }
            }
            Stmt::IoRead(name) => {
                prog.push(0, 1, InstID::READ);
                prog.push(1, 0, InstID::STORE(get_var(name, vars)))
            }
            Stmt::IoWrite(expr) => {
                expr.to_jvm(prog, vars);
                prog.push(1, 0, InstID::WRITE);
            }
            Stmt::Assignment { name, expr } => {
                expr.to_jvm(prog, vars);
                prog.push(1, 0, InstID::STORE(get_var(name, vars)));
            }
            Stmt::IfElse {
                cond,
                stmt,
                else_stmt,
            } => {
                cond.to_jvm(prog, vars);
                let else_label = get_label(prog);
                prog.push(1, 0, InstID::FJUMP(else_label.clone()));
                stmt.to_jvm(prog, vars);
                let end_label = get_label(prog);
                prog.push(0, 0, InstID::JUMP(end_label.clone()));
                prog.push(0, 0, InstID::JUMP_TARGET(else_label));
                else_stmt.to_jvm(prog, vars);
                prog.push(0, 0, InstID::JUMP_TARGET(end_label));
            }
            Stmt::If { cond, stmt } => {
                cond.to_jvm(prog, vars);
                let end_label = get_label(prog);
                prog.push(1, 0, InstID::FJUMP(end_label.clone()));
                stmt.to_jvm(prog, vars);
                prog.push(0, 0, InstID::JUMP_TARGET(end_label));
            }
            Stmt::While { cond, stmt } => {
                let start_label = get_label(prog);
                let end_label = get_label(prog);
                prog.push(0, 0, InstID::JUMP_TARGET(start_label.clone()));
                cond.to_jvm(prog, vars);
                prog.push(1, 0, InstID::FJUMP(end_label.clone()));
                stmt.to_jvm(prog, vars);
                prog.push(0, 0, InstID::JUMP(start_label));
                prog.push(0, 0, InstID::JUMP_TARGET(end_label));
            }
        }
    }
}

impl GenerateJvm for Cond {
    fn to_jvm(&self, prog: &mut JVMProgram, vars: &HashMap<String, usize>) {
        match self {
            Cond::True => {
                prog.push(0, 1, InstID::TRUE);
            }
            Cond::False => {
                prog.push(0, 1, InstID::FALSE);
            }
            Cond::Nop(cond) => {
                cond.to_jvm(prog, vars);
            }
            Cond::Comp { op, left, right } => {
                left.to_jvm(prog, vars);
                right.to_jvm(prog, vars);
                op.to_jvm(prog, vars);
            }
            Cond::Bunop { op, cond } => {
                cond.to_jvm(prog, vars);
                op.to_jvm(prog, vars);
            }
            Cond::BBinop { op, left, right } => {
                left.to_jvm(prog, vars);
                right.to_jvm(prog, vars);
                op.to_jvm(prog, vars);
            }
        }
    }
}

impl GenerateJvm for Bunop {
    fn to_jvm(&self, prog: &mut JVMProgram, _vars: &HashMap<String, usize>) {
        match self {
            Bunop::Neg => {
                prog.push(1, 1, InstID::NOT);
            }
        }
    }
}

impl GenerateJvm for BBinop {
    fn to_jvm(&self, prog: &mut JVMProgram, _vars: &HashMap<String, usize>) {
        match self {
            BBinop::And => {
                prog.push(2, 1, InstID::AND);
            }
            BBinop::Or => {
                prog.push(2, 1, InstID::OR);
            }
        }
    }
}

impl GenerateJvm for Compop {
    fn to_jvm(&self, prog: &mut JVMProgram, _vars: &HashMap<String, usize>) {
        match self {
            Compop::Eq => {
                prog.push(2, 1, InstID::EQ);
            }
            Compop::Neq => {
                prog.push(2, 1, InstID::NEQ);
            }
            Compop::Lte => {
                prog.push(2, 1, InstID::LEQ);
            }
            Compop::Gte => {
                prog.push(2, 1, InstID::GEQ);
            }
            Compop::Lt => {
                prog.push(2, 1, InstID::LESS);
            }
            Compop::Gt => {
                prog.push(2, 1, InstID::GREATER);
            }
        }
    }
}

impl GenerateJvm for Expr {
    fn to_jvm(&self, prog: &mut JVMProgram, vars: &HashMap<String, usize>) {
        match self {
            Expr::Number(n) => {
                prog.push(0, 1, InstID::CONST(*n));
            }
            Expr::Name(name) => prog.push(0, 1, InstID::LOAD(get_var(name, vars))),
            Expr::Expr(expr) => {
                expr.to_jvm(prog, vars);
            }
            Expr::Unop { op, expr } => {
                expr.to_jvm(prog, vars);
                op.to_jvm(prog, vars);
            }
            Expr::Binop { op, left, right } => {
                left.to_jvm(prog, vars);
                right.to_jvm(prog, vars);
                op.to_jvm(prog, vars);
            }
        }
    }
}

impl GenerateJvm for Unop {
    fn to_jvm(&self, prog: &mut JVMProgram, _vars: &HashMap<String, usize>) {
        match self {
            Unop::Minus => {
                prog.push(1, 1, InstID::NEG);
            }
        }
    }
}

impl GenerateJvm for Binop {
    fn to_jvm(&self, prog: &mut JVMProgram, _vars: &HashMap<String, usize>) {
        match self {
            Binop::Minus => {
                prog.push(2, 1, InstID::SUB);
            }
            Binop::Plus => {
                prog.push(2, 1, InstID::ADD);
            }
            Binop::Multilply => {
                prog.push(2, 1, InstID::MUL);
            }
            Binop::Divide => {
                prog.push(2, 1, InstID::DIV);
            }
            Binop::Modulo => {
                prog.push(2, 1, InstID::MOD);
            }
        }
    }
}

fn get_label(prog: &mut JVMProgram) -> String {
    let mut label = String::new();
    let mut i = prog.label_count;
    loop {
        label.push(((i % 26) as u8 + b'A') as char);
        i /= 26;
        if i == 0 {
            break;
        }
    }
    prog.label_count += 1;
    label
}

pub fn dump(ast: &Program) {
    let prog = ast2jvm(ast);
    for inst in prog.instructions {
        print!("{}", id2str(&inst.id));
        match inst.id {
            InstID::CONST(i) => {
                print!(" {}", i);
            }
            InstID::LOAD(i) | InstID::STORE(i)  => {
                print!(" {}", i-1);
            }
            InstID::ALLOC(i) => {
                print!(" {}", i);
            },
            InstID::JUMP(ref s) | InstID::FJUMP(ref s) => {
                print!(" {}", s);
            },
            InstID::JUMP_TARGET(ref s) => {
                print!("{}:", s);
            }
            _ => (),
        }
        println!();
    }
}

fn id2str(id: &InstID) -> &str {
    match id {
        InstID::NEG => "NEG",
        InstID::ADD => "ADD",
        InstID::SUB => "SUB",
        InstID::MUL => "MUL",
        InstID::DIV => "DIV",
        InstID::MOD => "MOD",
        InstID::NOT => "NOT",
        InstID::AND => "AND",
        InstID::OR => "OR",
        InstID::LESS => "LESS",
        InstID::LEQ => "LEQ",
        InstID::EQ => "EQ",
        InstID::NEQ => "NEQ",
        InstID::GREATER => "GREATER",
        InstID::GEQ => "GEQ",
        InstID::CONST(_) => "CONST",
        InstID::TRUE => "TRUE",
        InstID::FALSE => "FALSE",
        InstID::LOAD(_) => "LOAD",
        InstID::STORE(_) => "STORE",
        InstID::ALLOC(_) => "ALLOC",
        InstID::JUMP(_)=> "JUMP",
        InstID::FJUMP(_) => "FJUMP",
        InstID::READ => "READ",
        InstID::WRITE => "WRITE",
        InstID::HALT => "HALT",
        InstID::JUMP_TARGET(_) => "",
    }
}

fn get_var(name: &str, vars: &HashMap<String, usize>) -> usize {
    match vars.get(name) {
        Some(i) => *i,
        None => {
            eprintln!("Vars: {:?}\n Failed to look up variable '{}'", vars, name);
            panic!();
        },
    }
}
