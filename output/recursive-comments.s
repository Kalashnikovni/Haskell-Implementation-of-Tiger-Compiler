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
movq $0, %rax
jmp L6
L6:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
