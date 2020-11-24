.data
s: .word 0
c: .word 0
b: .word 0
a: .word 0
str_0: .asciiz "Hello"
.text
.globl main
g:
li $t2, 8
sub $sp, $sp, $t2
sw $ra, 4($sp)
li $t0, 1
lw $t1, b
sub $t0, $t0, $t1
sw $t0, 0($sp)
lw $a0, 0($sp)
li $v0, 1
syscall
j leave_g
leave_g:
lw $ra, 4($sp)
li $t2, 8
add $sp, $sp, $t2
jr $ra
main:
li $t2, 8
sub $sp, $sp, $t2
sw $ra, 4($sp)
li $t0, 0
sw $t0, a
lw $t0, a
li $t1, 1
add $t0, $t0, $t1
sw $t0, 0($sp)
lw $t0, 0($sp)
sw $t0, a
li $t0, 0
sw $t0, b
li $t0, 99
sw $t0, c
la $t0, str_0
sw $t0, s
lw $a0, s
li $v0, 4
syscall
leave_main:
lw $ra, 4($sp)
li $t2, 8
add $sp, $sp, $t2
li $v0, 10
syscall