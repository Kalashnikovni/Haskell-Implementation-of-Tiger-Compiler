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
movq %rbp, %rdi
movq $10, %r10
movq %r10, %rsi
call fact
jmp L18
L18:
jmp final
fact:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, %r11
movq $0, %r10
cmp %r11, %r10
je L3
jmp L4
L4:
movq %r11, -16(%rbp)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $1, %r10
subq %r10, %r11
movq %r11, %rsi
call fact
movq -16(%rbp), %r11
imul %rax, %r11
L5:
movq %r11, %rax
jmp L22
L3:
movq $1, %r11
jmp L5
L22:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
