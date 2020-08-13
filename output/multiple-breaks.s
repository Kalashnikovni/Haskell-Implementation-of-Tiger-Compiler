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
call loopbreaks
movq $0, %rax
jmp L18
L18:
jmp final
loopbreaks:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $3, %r8
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %r8, %r10
jle L5
jmp L1
L1:
jmp L20
L5:
jmp L1
L21:
jmp L1
L22:
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %r10, %r8
je L1
jmp L6
L6:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L5
L20:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
