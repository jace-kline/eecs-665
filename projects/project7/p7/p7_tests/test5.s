.data
gbl_a: .quad 0
gbl_b: .byte 0
gbl_cptr: .quad 0
str_0: .asciz "hello"
.align 8

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $0, %rsp
movq $5, %rax
movq %rax, (gbl_a)
movq $97, %rax
movb %al, (gbl_b)
leaq str_0, %rax
movq %rax, (gbl_cptr)
movq (gbl_a), %rdi
callq printInt
movq $10, %rdi
callq printByte
movzbq (gbl_b), %rdi
callq printByte
movq $10, %rdi
callq printByte
movq (gbl_cptr), %rdi
callq printString
lbl_0: addq $0, %rsp
popq %rbp
ret

