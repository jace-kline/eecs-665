.data
gbl_b: .byte 0
gbl_a: .quad 0
gbl_c: .byte 0

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $0, %rsp
movq $0, %rax
movb %al, (gbl_b)
movq $7, %rax
movq %rax, (gbl_a)
movq $122, %rax
movb %al, (gbl_c)
movzbq (gbl_b), %rdi
callq printByte
movq (gbl_a), %rdi
callq printInt
movzbq (gbl_c), %rdi
callq printByte
lbl_0: addq $0, %rsp
popq %rbp
ret

