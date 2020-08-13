.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $0, %r8
movq $100, %r9
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %r9, %r10
jle L6
jmp L2
L2:
movq $0, %rax
jmp L13
L6:
addq $1, %r8
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %r10, %r9
je L2
jmp L7
L7:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L6
L13:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
