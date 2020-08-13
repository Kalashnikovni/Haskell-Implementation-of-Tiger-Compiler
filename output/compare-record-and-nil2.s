.data
L2:
  .quad 4
 .string "hola"
  
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
movq $3, %r10
movq %r10, %rsi
movq $L2, %r10
movq %r10, %rdx
call _allocRecord
movq $0, %rax
movq $0, %r10
cmp %rax, %r10
je L7
jmp L7
L7:
movq $1, %r10
movq $0, %r11
cmp %rax, %r11
jne L5
jmp L6
L6:
movq $0, %r10
L5:
movq %r10, %rax
jmp L13
L13:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
