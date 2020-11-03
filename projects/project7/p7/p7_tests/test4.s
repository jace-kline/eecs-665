.data
str_0: .asciz "Hello, World!"
.align 8

.globl main
.text

main: pushq %rbp
movq %rsp, %rbp
addq $16, %rbp
subq $0, %rsp
leaq str_0, %rdi
callq printString
lbl_0: addq $0, %rsp
popq %rbp
ret

