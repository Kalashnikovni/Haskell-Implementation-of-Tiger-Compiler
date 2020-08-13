.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
addq $-168, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq $3, %r11
movq %r11, (%r10)
movq $4, %r10
movq %r10, -24(%rbp)
movq $5, %r10
movq %r10, -32(%rbp)
movq $6, %r10
movq %r10, -40(%rbp)
movq $7, %r10
movq %r10, -48(%rbp)
movq $8, %r10
movq %r10, -56(%rbp)
movq $9, %r10
movq %r10, -64(%rbp)
movq $10, %r10
movq %r10, -72(%rbp)
movq $11, %r10
movq %r10, -80(%rbp)
movq $12, %r10
movq %r10, -88(%rbp)
movq $13, %r10
movq %r10, -96(%rbp)
movq $14, %r10
movq %r10, -104(%rbp)
movq $15, %r10
movq %r10, -112(%rbp)
movq $16, %r10
movq %r10, -120(%rbp)
movq $17, %r10
movq %r10, -128(%rbp)
movq $18, %r10
movq %r10, -136(%rbp)
movq $19, %r10
movq %r10, -144(%rbp)
movq $20, %r10
movq %r10, -152(%rbp)
movq $21, %r10
movq %r10, -160(%rbp)
movq $22, %r10
movq %r10, -168(%rbp)
movq $23, %r10
movq %r10, -176(%rbp)
movq $24, %r10
movq %r10, -184(%rbp)
xorq %rax, %rax
movq %rbp, %rdi
movq $2, %r10
movq %r10, %rsi
movq $1, %r10
movq %r10, %rdx
movq $1, %r10
movq %r10, %rcx
movq $1, %r10
movq %r10, %r8
movq $1, %r10
movq %r10, %r9
call g
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq %r10, %rax
movq -24(%rbp), %r10
addq %r10, %rax
movq -32(%rbp), %r10
addq %r10, %rax
movq -40(%rbp), %r10
addq %r10, %rax
movq -48(%rbp), %r10
addq %r10, %rax
movq -56(%rbp), %r10
addq %r10, %rax
movq -64(%rbp), %r10
addq %r10, %rax
movq -72(%rbp), %r10
addq %r10, %rax
movq -80(%rbp), %r10
addq %r10, %rax
movq -88(%rbp), %r10
addq %r10, %rax
movq -96(%rbp), %r10
addq %r10, %rax
movq -104(%rbp), %r10
addq %r10, %rax
movq -112(%rbp), %r10
addq %r10, %rax
movq -120(%rbp), %r10
addq %r10, %rax
movq -128(%rbp), %r10
addq %r10, %rax
movq -136(%rbp), %r10
addq %r10, %rax
movq -144(%rbp), %r10
addq %r10, %rax
movq -152(%rbp), %r10
addq %r10, %rax
movq -160(%rbp), %r10
addq %r10, %rax
movq -168(%rbp), %r10
addq %r10, %rax
movq -176(%rbp), %r10
addq %r10, %rax
movq -184(%rbp), %r10
addq %r10, %rax
jmp L38
L38:
jmp final
g:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rdx, %rdi
movq %rcx, %rax
movq %r8, %r11
movq %r9, %r10
addq %rdi, %rsi
addq %rax, %rsi
addq %r11, %rsi
addq %r10, %rsi
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
addq %r10, %rsi
movq %rsi, %rax
jmp L71
L71:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
