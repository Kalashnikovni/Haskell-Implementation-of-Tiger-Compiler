.data
L1:
  .quad 6
 .string "Hello!"
  
.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq $L1, %r10
xorq %rax, %rax
movq %r10, %rdi
call print
movq $0, %rax
jmp L9
L9:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
