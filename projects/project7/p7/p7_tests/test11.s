.data

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $32, %rsp
movq $5, %rax
movq %rax, -24(%rbp)
leaq -24(%rbp), %rax
movq %rax, -40(%rbp)
movq -40(%rbp), %rax
movq %rax, -32(%rbp)
movq -32(%rbp), %rax
movq (%rax), %rax
movq %rax, -48(%rbp)
movq $6, %rax
movq %rax, -48(%rbp)
movq -32(%rbp), %rbx
movq %rax, (%rbx)
movq -24(%rbp), %rdi
callq printInt
lbl_0: addq $32, %rsp
popq %rbp
ret

