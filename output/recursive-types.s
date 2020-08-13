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
xorq %rax, %rax
movq $2, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
movq $0, %r10
movq %r10, %rdx
call _allocRecord
movq $0, %rax
jmp L10
L10:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
