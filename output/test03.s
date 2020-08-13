.data
L5:
  .quad 8
 .string "Somebody"
  L2:
  .quad 6
 .string "Nobody"
  
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
movq $1000, %r10
movq %r10, %rsi
movq $L2, %r10
movq %r10, %rdx
call _allocRecord
movq $1, %r10
movq $8, %r11
imul %r11, %r10
addq %r10, %rax
movq $L5, %r10
movq %r10, (%rax)
movq $0, %rax
jmp L11
L11:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
