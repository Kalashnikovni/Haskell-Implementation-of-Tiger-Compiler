.data
L2:
  .quad 2
 .string "a
"
  
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
jmp L20
L20:
jmp final
loopbreaks:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $3, %r10
movq %r10, -24(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -24(%rbp), %r10
cmp %r10, %r11
jle L7
jmp L1
L1:
jmp L22
L7:
xorq %rax, %rax
movq $L2, %r10
movq %r10, %rdi
call print
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -24(%rbp), %r10
cmp %r11, %r10
je L1
jmp L8
L8:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L7
L22:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
