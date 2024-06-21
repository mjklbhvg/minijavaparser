use std::arch::global_asm;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::mem::transmute;

use crate::types::*;
use crate::jvm::*;

// These functions don't actually use the C calling
// convention, and are only called by generated code.
extern "C" {
    fn __minijava_builtin_io_read();
    fn __minijava_builtin_io_write();
}

global_asm!(
r#"
    .global __minijava_builtin_io_read
    .global __minijava_builtin_io_write
    __minijava_builtin_io_read:
        push rbp
        mov rbp, rsp
        sub rsp, 4
        mov dword ptr [rsp], 0
        lea rsi, [rsp]
        xor rdi, rdi
        xor rbx, rbx
        io_read_loop:
        mov rdx, 1
        xor rax, rax
        syscall
        mov al, [rsp]
        cmp al, 0x0a
        je io_read_handle_sign
        cmp al, 0x2d
        jne io_append_char
        mov cx, [rbp-2]
        xor cx, 1
        mov [rbp-2], cx
        jmp io_read_loop
        io_append_char:
        imul rbx, 10
        movzx rax, al
        sub rax, 0x30
        add rbx, rax
        jmp io_read_loop
        io_read_handle_sign:
        mov cx, [rbp-2]
        test cx, cx
        jz io_read_ret
        neg rbx
        io_read_ret:
        mov rax, rbx
        leave
        ret

    __minijava_builtin_io_write:
        push rbp
        mov rbp, rsp
        push 0x0a
        xor rsi, rsi
        test rax, rax
        js io_write_neg
        jnz io_write_nonnull
        push 0x30
        jmp io_write_fmt_done
        io_write_neg:
        neg rax
        inc rsi
        io_write_nonnull:
        mov rcx, 10
        io_write_fmt_loop:
        xor rdx, rdx
        div rcx
        test rax, rax
        jnz io_write_putchar
        test dl, dl
        jz io_write_put_sign
        io_write_putchar:
        add dl, 0x30
        dec rsp
        mov [rsp], dl
        jmp io_write_fmt_loop
        io_write_put_sign:
        test rsi, rsi
        jz io_write_fmt_done
        push byte ptr 0x2d
        io_write_fmt_done:
        mov rdx, rbp
        sub rdx, rsp
        lea rsi, [rsp]
        mov rdi, 1
        mov rax, 1
        syscall
        leave
        ret
"#);

pub fn run(ast: &Program) {
    let mut code: Vec<u8> = vec![];
    let mut jump_targets = HashMap::new();
    let mut jump_locations = HashMap::new();

    let prog = ast2jvm(ast);

    // setup the stack
    push(&mut code, Reg::RBP);
    mov_rr(&mut code, Reg::RBP, Reg::RSP);

    let mut valid_regs = 0;
    for inst in prog.instructions {

        // If the instruction does not modify the stack,
        // we can defer potentially saving registers for
        // to the next instruction. Because of the way registers
        // carry over into the next instruction here, instructions
        // taking multiple arguments will have their arguments
        // in registers 'in reverse', e.g. Arg 1 -> rcx, Arg 2 -> rax
        // Or for 3 arguments: Arg 1 -> rdx, Arg 2 -> rcx, Arg 3 -> rax
        if inst.push != 0 || inst.pop != 0 {
            // The previous instruction pushes less values
            // than this one would pop. We only need to pop
            // the remaining values.
            if valid_regs < inst.pop {
                for r in valid_regs..inst.pop {
                    pop(&mut code, get_gpr(r));
                }
            } else {
                // The previous instruction pushes more values than
                // we actually need. We only need to push the remaining
                // values in reverse.
                // XXX: This doesn't make any difference at the moment,
                //      because no instruction pushes more than one value.
                if inst.pop < valid_regs {
                    for r in (inst.pop..valid_regs).rev() {
                        push(&mut code, get_gpr(r));
                    }
                }
            }
        }

        match inst.id {
            InstID::NEG => { neg(&mut code, Reg::RAX) },
            InstID::ADD => { add_rr(&mut code, Reg::RAX, Reg::RCX) },
            InstID::SUB => {
                sub_rr(&mut code, Reg::RCX, Reg::RAX);
                xchg_rr(&mut code, Reg::RCX, Reg::RAX);
            },
            InstID::MUL => { imul(&mut code, Reg::RCX) },
            InstID::DIV => {
                xor_rr(&mut code, Reg::RDX, Reg::RDX);
                xchg_rr(&mut code, Reg::RAX, Reg::RCX);
                idiv(&mut code, Reg::RCX);
            },
            InstID::MOD => {
                xor_rr(&mut code, Reg::RDX, Reg::RDX);
                xchg_rr(&mut code, Reg::RAX, Reg::RCX);
                idiv(&mut code, Reg::RCX);
                mov_rr(&mut code, Reg::RAX, Reg::RDX);
            },
            InstID::NOT => { not(&mut code, Reg::RAX) },
            InstID::AND => { and_rr(&mut code, Reg::RAX, Reg::RCX) },
            InstID::OR => { or_rr(&mut code, Reg::RAX, Reg::RCX) },
            InstID::LESS | InstID::LEQ | InstID::EQ | InstID::NEQ
                    | InstID::GREATER | InstID::GEQ => {
                compop_asm(&mut code, &inst.id);
            },
            InstID::CONST(i) => {
                if i <= 0x7fffffff {
                    mov_ri32(&mut code, Reg::RAX, i as i32)
                } else {
                    mov_ri(&mut code, Reg::RAX, i)
                }
            },
            InstID::TRUE => { mov_ri32(&mut code, Reg::RAX, 1) },
            InstID::FALSE => { mov_ri32(&mut code, Reg::RAX, 0) },
            InstID::LOAD(i) => {
                mov_rm(&mut code, Reg::RAX, Reg::RBP, -(i as i32)*8);
            },
            InstID::STORE(i) => {
                mov_mr(&mut code, Reg::RAX, Reg::RBP, -(i as i32)*8);
            },
            InstID::ALLOC(n) => { sub_ri(&mut code, Reg::RSP, (n as i32)*8) },
            InstID::JUMP(s) => {
                jmp(&mut code);
                jump_locations.insert(code.len(), s);
            },
            InstID::FJUMP(s) => {
                test_rr(&mut code, Reg::RAX, Reg::RAX);
                jz(&mut code);
                jump_locations.insert(code.len(), s);
            },
            InstID::READ => {
                mov_ri(&mut code, Reg::RSI, __minijava_builtin_io_read as u64);
                call(&mut code, Reg::RSI);
            },
            InstID::WRITE => {
                mov_ri(&mut code, Reg::RSI, __minijava_builtin_io_write as u64);
                call(&mut code, Reg::RSI);
            },
            InstID::HALT => {
                leave(&mut code);
                ret(&mut code)
            },
            InstID::JUMP_TARGET(s) => { jump_targets.insert(s, code.len()); },
        };

        if inst.push != 0 || inst.pop != 0 {
            valid_regs = inst.push;
        }
    }

    // Patch up the jump locations
    for (jump, target) in jump_locations.iter() {
        let rel = (*jump_targets.get(target).unwrap() as i32) - (*jump as i32);
        code[*jump - 4] = (rel & 0xff) as u8;
        code[*jump - 3] = ((rel >> 8) & 0xff) as u8;
        code[*jump - 2] = ((rel >> 16) & 0xff) as u8;
        code[*jump - 1] = ((rel >> 24) & 0xff) as u8;
    }

    // Save the generated code for debugging
    let mut f = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("a.out")
        .unwrap();
    let _ = f.write_all(&code);

    unsafe {
        let code_page = libc::mmap(
            transmute(0 as usize),
            code.len(), libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            0, 0
        );
        libc::memcpy(code_page, transmute(code.as_ptr()), code.len());
        libc::mprotect(code_page, code.len(), libc::PROT_READ | libc::PROT_EXEC);
        let callable: extern "C" fn() = transmute(code_page);
        callable();
        libc::munmap(code_page, code.len());
    };
}

fn compop_asm(code: &mut Vec<u8>, op: &InstID) {
    cmp_rr(code, Reg::RCX, Reg::RAX);
    match op {
        InstID::EQ       => { set_al(code, Cond::E) }
        InstID::NEQ      => { set_al(code, Cond::Ne) }
        InstID::LEQ      => { set_al(code, Cond::Le) }
        InstID::GEQ      => { set_al(code, Cond::Ge) }
        InstID::LESS     => { set_al(code, Cond::L) }
        InstID::GREATER  => { set_al(code, Cond::G) }
        _ => (),
    };
    and_ri(code, Reg::RAX, 0xff);
}

enum Reg {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
}

fn get_gpr(idx: u8) -> Reg {
    match idx {
        0 => Reg::RAX,
        1 => Reg::RCX,
        2 => Reg::RDX,
        3 => Reg::RBX,
        _ => panic!("ran out of general purpose registers :("),
    }
}

enum Cond {
    E  = 0x94,
    Ne = 0x95,
    L  = 0x9C,
    Ge = 0x9D,
    Le = 0x9E,
    G  = 0x9F,
}

macro_rules! mk_reg_inst {
    ($name: ident, $opcode: expr, $opcode_ext: expr) => {
        fn $name(code: &mut Vec<u8>, reg: Reg) {
            code.push(0x48);
            code.push($opcode);
            code.push((0b11 << 6) | ($opcode_ext << 3) | reg as u8);
        }
    }
}

macro_rules! mk_reg_reg_inst {
    ($name: ident, $opcode: expr) => {
        fn $name(code: &mut Vec<u8>, dst: Reg, src: Reg) {
            code.push(0x48);
            code.push($opcode);
            code.push((0b11 << 6) | ((dst as u8) << 3) | (src as u8));
        }
    }
}

macro_rules! mk_reg_imm_inst {
    ($name: ident, $opcode: expr, $opcode_ext: expr) => {
        fn $name(code: &mut Vec<u8>, dst: Reg, imm: i32) {
            code.push(0x48);
            code.push($opcode);
            code.push((0b11 << 6) | ($opcode_ext << 3) | dst as u8);
            code.append(&mut imm.to_le_bytes().to_vec());
        }
    }
}

mk_reg_inst!(push, 0xff, 0b110);
mk_reg_inst!(pop, 0x8f, 0b000);
mk_reg_inst!(neg, 0xf7, 0b011);
mk_reg_inst!(not, 0xf7, 0b010);
mk_reg_inst!(imul, 0xf7, 0b101);
mk_reg_inst!(idiv, 0xf7, 0b111);
mk_reg_inst!(call, 0xff, 0b010);
mk_reg_reg_inst!(add_rr, 0x03);
mk_reg_reg_inst!(xchg_rr, 0x87);
mk_reg_reg_inst!(sub_rr, 0x2b);
mk_reg_reg_inst!(cmp_rr, 0x3b);
mk_reg_reg_inst!(xor_rr, 0x33);
mk_reg_reg_inst!(test_rr, 0x85);
mk_reg_reg_inst!(and_rr, 0x23);
mk_reg_reg_inst!(or_rr, 0x0b);
mk_reg_imm_inst!(and_ri, 0x81, 0b100);
mk_reg_imm_inst!(sub_ri, 0x81, 0b101);
mk_reg_imm_inst!(mov_ri32, 0xC7, 0b000);

fn ret(code: &mut Vec<u8>) {
    code.push(0xc3);
}

fn leave(code: &mut Vec<u8>) {
    code.push(0xc9);
}

fn set_al(code: &mut Vec<u8>, cc: Cond) {
    code.append(&mut vec![0x0f, cc as u8, 0xc0]);
}

fn jmp(code: &mut Vec<u8>) {
    code.append(&mut vec![0xe9, 0xcc, 0xcc, 0xcc, 0xcc]);
}

fn jz(code: &mut Vec<u8>) {
    code.append(&mut vec![0x0f, 0x84, 0xcc, 0xcc, 0xcc, 0xcc]);
}

// Can't use the macro because src and dest swapped for some reason
fn mov_rr(code: &mut Vec<u8>, dst: Reg, src: Reg) {
    code.push(0x48);
    code.push(0x89);
    code.push((0b11 << 6) | ((src as u8) << 3) | (dst as u8));
}

fn mov_rm(code: &mut Vec<u8>, dst: Reg, base: Reg, disp: i32) {
    code.push(0x48);
    code.push(0x8B);
    code.push((0b10 << 6) | ((dst as u8) << 3) | (base as u8));
    code.append(&mut disp.to_le_bytes().to_vec());
}

fn mov_mr(code: &mut Vec<u8>, src: Reg, base: Reg, disp: i32) {
    code.push(0x48);
    code.push(0x89);
    code.push((0b10 << 6) | ((src as u8) << 3) | (base as u8));
    code.append(&mut disp.to_le_bytes().to_vec());
}

fn mov_ri(code: &mut Vec<u8>, dst: Reg, imm: u64) {
    code.push(0x48);
    code.push(0xB8 | dst as u8);
    code.append(&mut imm.to_le_bytes().to_vec());
}
