use crate::types::*;
use crate::jvm::*;

pub fn dump(ast: &Program) {
    // The output uses nasm syntax
    println!("section .text\nglobal _start\n_start:");

    // setup the stack
    println!("push rsp");
    println!("mov rbp, rsp");

    let prog = ast2jvm(ast);
    let mut valid_regs = 0;
    for inst in prog.instructions {

        // if the previous instruction wants to push
        // the same registers we are about to pop,
        // we can skip the whole thing!
        if valid_regs != inst.pop {
            for r in 0..inst.push {
                println!("push r{}x", (r + b'a') as char);
            }

            for r in (0..inst.pop).rev() {
                println!("pop r{}x", (r + b'a') as char);
            }
        }

        match inst.id {
            InstID::NEG => { println!("neg rax") },
            InstID::ADD => { println!("add rax, rbx") },
            InstID::SUB => { println!("sub rax, rbx") },
            InstID::MUL => { println!("imul rbx") },
            InstID::DIV => {
                println!("xor rdx, rdx");
                println!("idiv rbx");
            },
            InstID::MOD => {
                println!("xor rdx, rdx");
                println!("idiv rbx");
                println!("mov rax, rdx");
            },
            InstID::NOT => { println!("not rax") },
            InstID::AND => { println!("and rax, rbx") },
            InstID::OR => { println!("or rax, rbx") },
            InstID::LESS | InstID::LEQ | InstID::EQ | InstID::NEQ
                    | InstID::GREATER | InstID::GEQ => {
                compop_asm(&inst.id);
            },
            InstID::CONST(i) => { println!("mov rax, {}", i) },
            InstID::TRUE => { println!("mov rax, 1") },
            InstID::FALSE => { println!("mov rax, 0") },
            InstID::LOAD(i) => {
                println!("mov rax, [rbp-{}]", i*8)
            },
            InstID::STORE(i) => {
                println!("mov [rbp-{}], rax", i*8)
            },
            InstID::ALLOC(n) => { println!("sub rsp, {}", n*8) },
            InstID::JUMP(s) => { println!("jmp {}", s) },
            InstID::FJUMP(s) => {
                println!("test rax, rax");
                println!("jz {}", s);
            },
            InstID::READ => { println!("call io_read") },
            InstID::WRITE => { println!("call io_write") },
            InstID::HALT => {
                println!("mov rax, 60");
                println!("xor rdi, rdi");
                println!("syscall");
                println!("ret");
            },
            InstID::JUMP_TARGET(s) => { println!("{}:", s) },
        };

        valid_regs = inst.push;
    }

    println!(include_str!("rt.asm"));
}

fn compop_asm(op: &InstID) {
    println!("cmp rax, rbx");
    match op {
        InstID::EQ       => { println!("sete al"); }
        InstID::NEQ      => { println!("setne al"); }
        InstID::LEQ      => { println!("setle al"); }
        InstID::GEQ      => { println!("setge al"); }
        InstID::LESS     => { println!("setl al"); }
        InstID::GREATER  => { println!("setg al"); }
        _ => (),
    };
    println!("movzx ax, al");
    println!("movzx rax, ax");
}

