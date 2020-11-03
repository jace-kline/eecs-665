.data
gbl_a: .quad 0

.globl main
.text

lbl_fun_myFunc: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $0, %rsp
movq $5, %rax
jmp lbl_0
lbl_0: addq $0, %rsp
popq %rbp
ret

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $8, %rsp
call lbl_fun_myFunc
movq %rax, -24(%rbp)
movq -24(%rbp), %rax
movq %rax, (gbl_a)
movq (gbl_a), %rdi
callq printInt
movq $0, %rax
jmp lbl_1
lbl_1: addq $8, %rsp
popq %rbp
ret

