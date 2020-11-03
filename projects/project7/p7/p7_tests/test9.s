.data
str_0: .asciz "false"
.align 8
str_1: .asciz "true"
.align 8

.globl main
.text

lbl_fun_myFunc: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $88, %rsp
movq %rdi, -24(%rbp)
movq %rsi, -32(%rbp)
movq %rdx, -40(%rbp)
movq %rcx, -48(%rbp)
movq %r8, -56(%rbp)
movq %r9, -64(%rbp)
movq -24(%rbp), %rax
movq -32(%rbp), %rbx
addq %rbx, %rax
movq %rax, -72(%rbp)
movq -72(%rbp), %rax
movq -40(%rbp), %rbx
addq %rbx, %rax
movq %rax, -80(%rbp)
movq -80(%rbp), %rax
movq -48(%rbp), %rbx
addq %rbx, %rax
movq %rax, -88(%rbp)
movq -88(%rbp), %rax
movq -56(%rbp), %rbx
addq %rbx, %rax
movq %rax, -96(%rbp)
movq -96(%rbp), %rax
movq -64(%rbp), %rbx
addq %rbx, %rax
movq %rax, -104(%rbp)
movq -104(%rbp), %rax
jmp lbl_0
lbl_0: addq $88, %rsp
popq %rbp
ret

lbl_fun_toInt: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $8, %rsp
movb %dil, -24(%rbp)
movq $0, %rax
movzbq -24(%rbp), %rax
cmp $0, %rax
je lbl_2
movq $1, %rax
jmp lbl_1
lbl_2: nop
movq $0, %rax
jmp lbl_1
lbl_1: addq $8, %rsp
popq %rbp
ret

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $32, %rsp
movq $1, %rdi
movq $1, %rsi
movq $1, %rdx
movq $1, %rcx
movq $1, %r8
movq $1, %r9
call lbl_fun_myFunc
movq %rax, -32(%rbp)
movq -32(%rbp), %rax
movq $6, %rbx
cmpq %rbx, %rax
sete %al
movb %al, -40(%rbp)
movzbq -40(%rbp), %rax
movb %al, -24(%rbp)
movq $0, %rax
movzbq -24(%rbp), %rax
cmp $0, %rax
je lbl_4
leaq str_1, %rdi
callq printString
jmp lbl_5
lbl_4: nop
leaq str_0, %rdi
callq printString
lbl_5: nop
movzbq -24(%rbp), %rdi
call lbl_fun_toInt
movq %rax, -48(%rbp)
movq -48(%rbp), %rax
jmp lbl_3
lbl_3: addq $32, %rsp
popq %rbp
ret

