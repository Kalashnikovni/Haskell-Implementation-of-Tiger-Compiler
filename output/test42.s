.data
L17:
  .quad 4
 .string "kati"
  L7:
  .quad 9
 .string "somewhere"
  L6:
  .quad 5
 .string "aname"
  
.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-24, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %r12, -16(%rbp)
movq %r13, -24(%rbp)
movq %r14, -32(%rbp)
xorq %rax, %rax
movq $10, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, %r12
movq $5, %r13
xorq %rax, %rax
movq $4, %r10
movq %r10, %rdi
movq $L7, %r10
movq %r10, %rsi
movq $0, %r10
movq %r10, %rdx
movq $0, %r10
movq %r10, %rcx
movq $L6, %r10
movq %r10, %r8
call _allocRecord
movq %rax, %r10
xorq %rax, %rax
movq %r13, %rdi
movq %r10, %rsi
call _initArray
movq %rax, %r13
movq $0, %r14
xorq %rax, %rax
movq %r12, %rdi
movq %r14, %rsi
call _checkIndexArray
movq $8, %r10
imul %r10, %r14
movq %r12, %r10
addq %r14, %r10
movq $1, %r11
movq %r11, (%r10)
movq $9, %r14
xorq %rax, %rax
movq %r12, %rdi
movq %r14, %rsi
call _checkIndexArray
movq $8, %r10
imul %r10, %r14
addq %r14, %r12
movq $3, %r10
movq %r10, (%r12)
movq $3, %r12
xorq %rax, %rax
movq %r13, %rdi
movq %r12, %rsi
call _checkIndexArray
movq $8, %r10
imul %r10, %r12
movq %r13, %r10
addq %r12, %r10
movq (%r10), %r10
movq $3, %r11
movq $8, %r12
imul %r12, %r11
addq %r11, %r10
movq $L17, %r11
movq %r11, (%r10)
movq $1, %r12
xorq %rax, %rax
movq %r13, %rdi
movq %r12, %rsi
call _checkIndexArray
movq $8, %r10
imul %r10, %r12
movq %r13, %r10
addq %r12, %r10
movq (%r10), %r10
movq $1, %r11
movq $8, %r12
imul %r12, %r11
addq %r11, %r10
movq $23, %r11
movq %r11, (%r10)
movq $0, %rax
movq -16(%rbp), %r12
movq -24(%rbp), %r13
movq -32(%rbp), %r14
jmp L28
L28:
jmp final

final:
movq %rbp, %rsp
popq %rbp
ret
