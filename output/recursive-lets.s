.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $3, %r10
movq %r10, -16(%rbp)
movq $10, %r10
movq $11, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %r10
movq %r10, %rsi
call f
movq -16(%rbp), %r10
addq %r10, %rax
jmp L29
L29:
jmp final
f:
pushq %rbp
movq %rsp, %rbp
subq $32, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq %rsi, (%r10)
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, (%r11)
xorq %rax, %rax
movq %rbp, %rdi
movq $10, %r10
movq %r10, %rsi
call g
jmp L32
L32:
movq %rbp, %rsp
popq %rbp
ret
g:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq %rsi, (%r10)
movq $15, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq $3, %r10
movq %r10, %rsi
call h
jmp L39
L39:
movq %rbp, %rsp
popq %rbp
ret
h:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, %rcx
movq %rbx, %r9
movq %r12, %r8
movq %r14, %r11
movq %r15, %r10
movq %rbp, %r12
addq $-8, %r12
movq (%r12), %r12
addq $-8, %r12
movq (%r12), %r12
addq $-16, %r12
movq (%r12), %rax
movq %rbp, %r12
addq $-8, %r12
movq (%r12), %r12
addq $-16, %r12
movq (%r12), %r12
addq %r12, %rax
addq %rcx, %rax
movq %rbp, %r12
addq $-8, %r12
movq (%r12), %r12
addq $-8, %r12
movq (%r12), %r12
addq $-16, %r12
movq (%r12), %r12
addq %r12, %rax
movq %r9, %rbx
movq %r8, %r12
movq %r11, %r14
movq %r10, %r15
jmp L43
L43:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
