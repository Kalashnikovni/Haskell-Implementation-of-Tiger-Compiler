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
xorq %rax, %rax
movq $1, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _allocRecord
movq %rax, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq %r10, %rsi
call a
movq $0, %rax
jmp L18
L18:
jmp final
a:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $1, %r10
movq %rsi, %r11
xorq %rax, %rax
movq %r10, %rdi
movq $0, %r10
movq $8, %r8
imul %r8, %r10
addq %r10, %r11
movq (%r11), %r10
movq %r10, %rsi
call _allocRecord
jmp L23
L23:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
