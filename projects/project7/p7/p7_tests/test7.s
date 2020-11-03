.data

.globl main
.text

lbl_fun_fib: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $64, %rsp
movq %rdi, -24(%rbp)
movq -24(%rbp), %rax
movq $0, %rbx
cmpq %rbx, %rax
sete %al
movb %al, -32(%rbp)
movq $0, %rax
movzbq -32(%rbp), %rax
cmp $0, %rax
je lbl_1
movq $0, %rax
jmp lbl_0
lbl_1: nop
movq -24(%rbp), %rax
movq $1, %rbx
cmpq %rbx, %rax
sete %al
movb %al, -40(%rbp)
movq $0, %rax
movzbq -40(%rbp), %rax
cmp $0, %rax
je lbl_2
movq $1, %rax
jmp lbl_0
lbl_2: nop
movq -24(%rbp), %rax
movq $1, %rbx
subq %rbx, %rax
movq %rax, -48(%rbp)
movq -48(%rbp), %rdi
call lbl_fun_fib
movq %rax, -56(%rbp)
movq -24(%rbp), %rax
movq $2, %rbx
subq %rbx, %rax
movq %rax, -64(%rbp)
movq -64(%rbp), %rdi
call lbl_fun_fib
movq %rax, -72(%rbp)
movq -56(%rbp), %rax
movq -72(%rbp), %rbx
addq %rbx, %rax
movq %rax, -80(%rbp)
movq -80(%rbp), %rax
jmp lbl_0
lbl_0: addq $64, %rsp
popq %rbp
ret

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $16, %rsp
movq $10, %rdi
call lbl_fun_fib
movq %rax, -32(%rbp)
movq -32(%rbp), %rax
movq %rax, -24(%rbp)
movq -24(%rbp), %rdi
callq printInt
movq -24(%rbp), %rax
jmp lbl_3
lbl_3: addq $16, %rsp
popq %rbp
ret

