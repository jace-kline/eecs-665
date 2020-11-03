.data
gbl_c: .byte 0
gbl_b: .byte 0

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $0, %rsp
movq $97, %rax
movb %al, (gbl_c)
movq $0, %rax
movb %al, (gbl_b)
movzbq (gbl_c), %rdi
callq printByte
movzbq (gbl_b), %rdi
callq printByte
movq $0, %rax
jmp lbl_0
lbl_0: addq $0, %rsp
popq %rbp
ret

