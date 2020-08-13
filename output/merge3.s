.data
L76:
  .quad 1
 .string "0"
  L72:
  .quad 1
 .string "-"
  L61:
  .quad 1
 .string "0"
  L48:
  .quad 1
 .string "0"
  L25:
  .quad 1
 .string "
"
  L24:
  .quad 1
 .string " "
  L10:
  .quad 1
 .string "9"
  L6:
  .quad 1
 .string "0"
  
.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq %r10, -24(%rbp)
xorq %rax, %rax
call getstr
movq -24(%rbp), %r10
movq %rax, (%r10)
xorq %rax, %rax
movq $1, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _allocRecord
movq %rax, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq %r10, %rsi
call readint
movq %rax, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq %r10, %rsi
call printint
movq $0, %rax
jmp L99
L99:
jmp final
printint:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, -16(%rbp)
movq $0, %r10
movq -16(%rbp), %rsi
cmp %r10, %rsi
jl L81
jmp L82
L82:
movq $0, %r10
movq -16(%rbp), %rsi
cmp %r10, %rsi
jg L78
jmp L79
L79:
xorq %rax, %rax
movq $L76, %r10
movq %r10, %rdi
call print
L80:
L83:
jmp L104
L81:
xorq %rax, %rax
movq $L72, %r10
movq %r10, %rdi
call print
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L83
L78:
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L80
L104:
movq %rbp, %rsp
popq %rbp
ret
f:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, %r10
movq %r10, -16(%rbp)
movq $0, %r11
movq -16(%rbp), %r10
cmp %r11, %r10
jg L65
jmp L66
L66:
jmp L111
L65:
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $10, %r11
movq -16(%rbp), %r10
movq %r10, %rax
xorq %rdx, %rdx
idiv %r11
movq %rax, %rsi
call f
movq $10, %r11
movq -16(%rbp), %r10
movq %r10, %rax
xorq %rdx, %rdx
idiv %r11
movq $10, %r10
movq %rax, %r11
imul %r10, %r11
movq -16(%rbp), %r10
subq %r11, %r10
movq %r10, -24(%rbp)
xorq %rax, %rax
movq $L61, %r10
movq %r10, %rdi
call ord
movq %rax, %r11
xorq %rax, %rax
movq -24(%rbp), %r10
addq %r11, %r10
movq %r10, %rdi
call chr
movq %rax, %r10
xorq %rax, %rax
movq %r10, %rdi
call print
jmp L66
L111:
movq %rbp, %rsp
popq %rbp
ret
readint:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-48, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, -16(%rbp)
movq $0, %r10
movq %r10, -24(%rbp)
xorq %rax, %rax
movq %rbp, %rdi
call skipto
movq -16(%rbp), %rsi
movq %rsi, %r10
movq $0, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq %r10, -32(%rbp)
xorq %rax, %rax
movq %rbp, %rdi
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rsi
call isdigit
movq -32(%rbp), %r10
movq %rax, (%r10)
L51:
xorq %rax, %rax
movq %rbp, %rdi
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rsi
call isdigit
movq $0, %r10
cmp %rax, %r10
jne L52
jmp L46
L46:
movq -24(%rbp), %r10
movq %r10, %rax
jmp L129
L52:
movq $10, %r11
movq -24(%rbp), %r10
imul %r11, %r10
movq %r10, -40(%rbp)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rdi
call ord
movq -40(%rbp), %r10
addq %rax, %r10
movq %r10, -48(%rbp)
xorq %rax, %rax
movq $L48, %r10
movq %r10, %rdi
call ord
movq -48(%rbp), %r10
subq %rax, %r10
movq %r10, -24(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq %r10, -56(%rbp)
xorq %rax, %rax
call getstr
movq -56(%rbp), %r10
movq %rax, (%r10)
jmp L51
L129:
movq %rbp, %rsp
popq %rbp
ret
skipto:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-16, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
L35:
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $L24, %r10
movq %r10, %rsi
call _stringCompare
movq $0, %r10
cmp %rax, %r10
je L29
jmp L30
L30:
movq $1, %r10
movq %r10, -16(%rbp)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $L25, %r10
movq %r10, %rsi
call _stringCompare
movq $0, %r10
cmp %rax, %r10
je L27
jmp L28
L28:
movq $0, %r10
movq %r10, -16(%rbp)
L27:
movq -16(%rbp), %r10
L31:
movq $0, %r11
cmp %r10, %r11
jne L36
jmp L33
L33:
jmp L159
L29:
movq $1, %r10
jmp L31
L36:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq %r10, -24(%rbp)
xorq %rax, %rax
call getstr
movq -24(%rbp), %r10
movq %rax, (%r10)
jmp L35
L159:
movq %rbp, %rsp
popq %rbp
ret
isdigit:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-32, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, -16(%rbp)
xorq %rax, %rax
movq $L6, %r10
movq %r10, %rdi
call ord
movq %rax, %r10
movq %r10, -24(%rbp)
xorq %rax, %rax
movq -16(%rbp), %rsi
movq %rsi, %rdi
call ord
movq -24(%rbp), %r10
cmp %rax, %r10
jle L15
jmp L16
L16:
movq $0, %r10
L17:
movq %r10, %rax
jmp L185
L15:
movq $1, %r10
movq %r10, -32(%rbp)
xorq %rax, %rax
movq -16(%rbp), %rsi
movq %rsi, %rdi
call ord
movq %rax, %r10
movq %r10, -40(%rbp)
xorq %rax, %rax
movq $L10, %r10
movq %r10, %rdi
call ord
movq -40(%rbp), %r10
cmp %rax, %r10
jle L13
jmp L14
L14:
movq $0, %r10
movq %r10, -32(%rbp)
L13:
movq -32(%rbp), %r10
jmp L17
L185:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
