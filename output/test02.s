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
movq $10, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq $0, %rax
jmp L9
L9:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
