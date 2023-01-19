use crate::types::*;
use std::collections::HashMap;

trait GenerateAsm {
    fn to_asm(&self, variables: &HashMap<String, usize>);
}

pub fn dump(prog: &Program) {
    println!("section .text\nglobal _start\n_start:");
    println!("push rsp");
    println!("mov rbp, rsp");

    let mut variables = HashMap::<String, usize>::new();
    let mut offset = 0;
    for decl in &prog.decls {
        for ident in &decl.idents {
            offset += 8;
            variables.insert(ident.to_owned(), offset);
        }
    }
    println!("sub rsp, {}", offset);

    for stmt in &prog.stmts {
        stmt.to_asm(&variables);
    }

    println!("mov rax, 60");
    println!("xor rdi, rdi");
    println!("syscall");

    println!(
"; -------------- rt io functions -----------
io_read:
dec rsp
xor rdi, rdi
lea rsi, [rsp]
mov rdx, 1
xor rbx, rbx
io_read_loop:
xor rax, rax
syscall
mov cl, [rsp]
cmp cl, 10
je io_read_ret
imul rbx, 10
movzx rcx, cl
sub rcx, 0x30
add rbx, rcx
jmp io_read_loop
io_read_ret:
inc rsp
mov rax, rbx
ret
io_write:
push rbp
mov rbp, rsp
dec rsp
mov [rsp], byte 0x0a
test rax, rax
jnz io_write_nonnull
dec rsp
mov [rsp], byte 0x30
jmp io_write_fmt_done
io_write_nonnull:
mov rcx, 10
io_write_fmt_loop:
xor rdx, rdx
div rcx
test rax, rax
jnz io_write_putchar
test dl, dl
jnz io_write_putchar
jmp io_write_fmt_done
io_write_putchar:
add dl, 0x30
dec rsp
mov [rsp], dl
jmp io_write_fmt_loop
io_write_fmt_done:
mov rdx, rbp
sub rdx, rsp ; count
lea rsi, [rsp]
mov rdi, 1
mov rax, 1
syscall
mov rsp, rbp
pop rbp
ret");
}

impl GenerateAsm for Stmt {
    fn to_asm(&self, variables: &HashMap<String, usize>) {
        match self {
            Stmt::Nop => (),
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    stmt.to_asm(variables);
                }
            },
            Stmt::IoRead(name) => {
                println!("call io_read");
                println!("mov [rbp-{}], rax", variables.get(name).unwrap());
            },
            Stmt::IoWrite(expr) => {
                expr.to_asm(variables);
                println!("pop rax");
                println!("call io_write");
            },
            Stmt::Assignment { name, expr } => {
                expr.to_asm(variables);
                println!("pop rax");
                println!("mov [rbp-{}], rax", variables.get(name).unwrap());
            },
            Stmt::If { cond, stmt } => {
                cond.to_asm(variables);
                println!("pop rax");
                println!("test rax, rax");
                println!("jz label{}", (self as *const Stmt) as usize);
                stmt.to_asm(variables);
                println!("label{}:", (self as *const Stmt) as usize);
            },
            Stmt::IfElse { cond, stmt, else_stmt } => {
                cond.to_asm(variables);
                println!("pop rax");
                println!("test rax, rax");
                println!("jz label{}", (cond as *const Cond) as usize);
                stmt.to_asm(variables);
                println!("jmp label{}", (self as *const Stmt) as usize);
                println!("label{}:", (cond as *const Cond) as usize);
                else_stmt.to_asm(variables);
                println!("label{}:", (self as *const Stmt) as usize);
            },
            Stmt::While { cond, stmt } => {
                println!("label{}:", (cond as *const Cond) as usize);
                cond.to_asm(variables);
                println!("pop rax");
                println!("test rax, rax");
                println!("jz label{}", (self as *const Stmt) as usize);
                stmt.to_asm(variables);
                println!("jmp label{}", (cond as *const Cond) as usize);
                println!("label{}:", (self as *const Stmt) as usize);
            },
        }
    }
}

impl GenerateAsm for Cond {
    fn to_asm(&self, variables: &HashMap<String, usize>) {
        match self {
            Cond::True => {
                println!("push qword 1");
            },
            Cond::False => {
                println!("push qword 0");
            },
            Cond::Nop(cond) => {
                cond.to_asm(variables);
            },
            Cond::Comp { op, left, right } => {
                left.to_asm(variables);
                right.to_asm(variables);
                op.to_asm(variables);
            },
            Cond::Bunop { op, cond } => {
                cond.to_asm(variables);
                op.to_asm(variables);
            },
            Cond::BBinop { op, left, right } => {
                left.to_asm(variables);
                right.to_asm(variables);
                op.to_asm(variables);
            },
        };
    }
}

impl GenerateAsm for Bunop {
    fn to_asm(&self, _variables: &HashMap<String, usize>) {
        println!("pop rax");
        match self {
            Bunop::Neg => {
                println!("not rax");
            },
        };
        println!("push rax");
    }
}

impl GenerateAsm for BBinop {
    fn to_asm(&self, _variables: &HashMap<String, usize>) {
        println!("pop rbx");
        println!("pop rax");
        match self {
            BBinop::And => {
                println!("and rax, rbx");
            },
            BBinop::Or => {
                println!("or rax, rbx");
            },
        };
        println!("push rax");
    }
}

impl GenerateAsm for Compop {
    fn to_asm(&self, _variables: &HashMap<String, usize>) {
        println!("pop rbx");
        println!("pop rax");
        println!("cmp rax, rbx");
        match self {
            Compop::Eq => {
                println!("sete al");
            },
            Compop::Neq => {
                println!("setne al");
            },
            Compop::Lte => {
                println!("setle al");
            },
            Compop::Gte => {
                println!("setge al");
            },
            Compop::Lt => {
                println!("setl al");
            },
            Compop::Gt => {
                println!("setg al");
            },
        };
        println!("movzx ax, al");
        println!("movzx rax, ax");
        println!("push rax");
    }
}

impl GenerateAsm for Expr {
    fn to_asm(&self, variables: &HashMap<String, usize>) {
        match self {
            Expr::Number(n) => {
                println!("push qword {}", n);
            },
            Expr::Name(n) => {
                println!("mov rax, [rbp-{}]", variables.get(n).unwrap());
                println!("push rax");
            },
            Expr::Expr(expr) => { expr.to_asm(variables) },
            Expr::Unop { op, expr } => {
                expr.to_asm(variables);
                op.to_asm(variables);
            },
            Expr::Binop { op, left, right } => {
                left.to_asm(variables);
                right.to_asm(variables);
                op.to_asm(variables);
            },
        }
    }
}

impl GenerateAsm for Binop {
    fn to_asm(&self, _variables: &HashMap<String, usize>) {
        println!("pop rbx");
        println!("pop rax");
        match self {
            Binop::Minus => {
                println!("sub rax, rbx");
            },
            Binop::Plus => {
                println!("add rax, rbx");
            },
            Binop::Multilply => {
                println!("imul rbx");
            },
            Binop::Divide => {
                println!("xor rdx, rdx");
                println!("idiv rbx");
            },
            Binop::Modulo => {
                println!("xor rdx, rdx");
                println!("idiv rbx");
                println!("mov rax, rdx");
            },
        };
        println!("push rax");
    }
}

impl GenerateAsm for Unop {
    fn to_asm(&self, _variables: &HashMap<String, usize>) {
        println!("pop rax");
        match self {
            Unop::Minus => {
                println!("neg rax");
            },
        };
        println!("push rax");
    }
}
