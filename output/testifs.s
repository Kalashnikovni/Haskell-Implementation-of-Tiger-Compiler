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
movq $1, %r10
movq $10, %r8
movq $20, %r11
cmp %r8, %r11
je L2
jmp L3
L3:
movq $0, %r10
L2:
movq $0, %r11
cmp %r10, %r11
jne L7
jmp L8
L8:
movq $0, %r10
L9:
movq $0, %r11
cmp %r10, %r11
jne L14
jmp L15
L15:
movq $0, %r10
L16:
movq $0, %r11
cmp %r10, %r11
jne L18
jmp L19
L19:
movq $40, %rax
L20:
jmp L27
L7:
movq $1, %r10
movq $5, %r8
movq $6, %r11
cmp %r8, %r11
je L5
jmp L6
L6:
movq $0, %r10
L5:
jmp L9
L14:
movq $1, %r10
movq $4, %r8
movq $3, %r11
cmp %r8, %r11
je L12
jmp L13
L13:
movq $0, %r10
L12:
jmp L16
L18:
movq $30, %rax
jmp L20
L27:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
