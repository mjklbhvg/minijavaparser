; -------------- rt io functions -----------
io_read:
push rbp
mov rbp, rsp
sub rsp, 4
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
mov cx, word [rbp-2]
xor cx, 1
mov word [rbp-2], cx
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

io_write:
push rbp
mov rbp, rsp
push byte 0x0a
xor rsi, rsi
test rax, rax
js io_write_neg
jnz io_write_nonnull
push byte 0x30
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
push byte 0x2d
io_write_fmt_done:
mov rdx, rbp
sub rdx, rsp
lea rsi, [rsp]
mov rdi, 1
mov rax, 1
syscall
leave
ret
