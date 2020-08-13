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
movq $0, %r10
movq $0, %r11
cmp %r10, %r11
je L6
jmp L6
L6:
movq $1, %rax
movq $0, %r11
cmp %r10, %r11
jne L4
jmp L5
L5:
movq $0, %rax
L4:
jmp L12
L12:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
