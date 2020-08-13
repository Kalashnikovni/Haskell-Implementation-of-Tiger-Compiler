.data
L2:
  .quad 1
 .string " "
  
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
movq $L2, %r10
movq $0, %rax
jmp L9
L9:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
