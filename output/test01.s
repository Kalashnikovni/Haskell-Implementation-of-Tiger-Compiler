.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %r12, -16(%rbp)
movq %r13, -24(%rbp)
xorq %rax, %rax
movq $10, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, %r13
movq $2, %r12
xorq %rax, %rax
movq %r13, %rdi
movq %r12, %rsi
call _checkIndexArray
movq $8, %r10
imul %r10, %r12
addq %r12, %r13
movq (%r13), %r10
movq $0, %rax
movq -16(%rbp), %r12
movq -24(%rbp), %r13
jmp L12
L12:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
