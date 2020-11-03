.data

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $48, %rsp
movq $5, %rax
movq %rax, -24(%rbp)
leaq -24(%rbp), %rax
movq %rax, -48(%rbp)
movq -48(%rbp), %rax
movq %rax, -32(%rbp)
movq -32(%rbp), %rax
movq (%rax), %rax
movq %rax, -56(%rbp)
movq -56(%rbp), %rax
movq $5, %rbx
addq %rbx, %rax
movq %rax, -64(%rbp)
movq -64(%rbp), %rax
movq %rax, -40(%rbp)
movq -40(%rbp), %rdi
callq printInt
movq -40(%rbp), %rax
jmp lbl_0
lbl_0: addq $48, %rsp
popq %rbp
ret

