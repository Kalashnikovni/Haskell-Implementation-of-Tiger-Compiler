.data

.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
xorq %rax, %rax
movq %rbp, %rdi
call loopbreaks
movq $0, %rax
jmp L27
L27:
jmp final
loopbreaks:
pushq %rbp
movq %rsp, %rbp
subq $32, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $3, %rax
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %rax, %r10
jle L14
jmp L1
L1:
jmp L31
L14:
movq $4, %r9
movq %rbp, %r10
addq $-24, %r10
movq $1, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
cmp %r9, %r10
jle L9
jmp L2
L2:
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
cmp %r10, %rax
je L1
jmp L15
L15:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L14
L9:
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r11
movq $2, %r10
cmp %r10, %r11
jg L3
jmp L4
L4:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r8
addq $-16, %r8
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r11
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
addq %r10, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq %r10, %r11
movq %r11, (%r8)
L5:
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
cmp %r10, %r9
je L2
jmp L10
L10:
movq %rbp, %r11
addq $-24, %r11
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L9
L3:
jmp L2
L32:
jmp L5
L31:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
