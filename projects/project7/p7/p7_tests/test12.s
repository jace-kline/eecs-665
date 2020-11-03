.data
gbl_cp: .quad 0

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $56, %rsp
movq $97, %rax
movb %al, -32(%rbp)
movq $98, %rax
movb %al, -40(%rbp)
leaq -32(%rbp), %rax
movq %rax, -48(%rbp)
movq -48(%rbp), %rax
movq %rax, -24(%rbp)
leaq -40(%rbp), %rax
movq %rax, -56(%rbp)
movq -56(%rbp), %rax
movq %rax, (gbl_cp)
movq (gbl_cp), %rax
movq (%rax), %rax
movq %rax, -64(%rbp)
movq -24(%rbp), %rax
movq (%rax), %rax
movq %rax, -72(%rbp)
movq -64(%rbp), %rax
movq %rax, -72(%rbp)
movq -24(%rbp), %rbx
movq %rax, (%rbx)
movzbq -32(%rbp), %rdi
callq printByte
lbl_0: addq $56, %rsp
popq %rbp
ret

