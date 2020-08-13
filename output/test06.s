.data
L21:
  .quad 4
 .string "str2"
  L14:
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
movq $L21, %r10
movq %r10, %rdx
call do_nothing1
movq $1, %rax
jmp L28
L28:
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
movq $L14, %r10
movq %r10, %rdx
call do_nothing1
jmp L32
L32:
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
movq $0, %r10
cmp %rsi, %r10
je L4
jmp L5
L5:
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $1, %r10
subq %r10, %rsi
movq %rsi, %rsi
call do_nothing2
L6:
jmp L37
L4:
movq $23, %rax
jmp L6
L37:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
