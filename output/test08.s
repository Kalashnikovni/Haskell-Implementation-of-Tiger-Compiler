.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $1, %r10
movq $10, %r8
movq $20, %r11
cmp %r8, %r11
je L2
jmp L3
L3:
movq $0, %r10
L2:
movq $0, %r11
cmp %r10, %r11
jne L4
jmp L5
L5:
movq $40, %rax
L6:
jmp L13
L4:
movq $30, %rax
jmp L6
L13:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
