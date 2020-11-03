.data

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $8, %rsp
movq $5, %rax
movq %rax, -24(%rbp)
movq -24(%rbp), %rdi
callq printInt
movq $0, %rax
jmp lbl_0
lbl_0: addq $8, %rsp
popq %rbp
ret

