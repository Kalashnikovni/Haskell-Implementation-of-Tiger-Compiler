.data
L18:
  .quad 4
 .string "str2"
  L12:
  .quad 1
 .string " "
  L10:
  .quad 3
 .string "str"
  
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
movq $0, %r10
movq %r10, %rsi
movq $L18, %r10
movq %r10, %rdx
call do_nothing1
jmp L25
L25:
jmp final
do_nothing2:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq %rsi, %rsi
movq $L10, %r10
movq %r10, %rdx
call do_nothing1
movq $L12, %rax
jmp L29
L29:
movq %rbp, %rsp
popq %rbp
ret
do_nothing1:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
addq $1, %rsi
movq %rsi, %rsi
call do_nothing2
movq $0, %rax
jmp L35
L35:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
