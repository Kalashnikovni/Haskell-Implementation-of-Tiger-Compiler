.data
L109:
  .quad 1
 .string " "
  L105:
  .quad 1
 .string "
"
  L91:
  .quad 1
 .string "0"
  L87:
  .quad 1
 .string "-"
  L76:
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
movq %r12, -24(%rbp)
movq %rbp, %r12
addq $-16, %r12
xorq %rax, %rax
call getstr
movq %rax, (%r12)
xorq %rax, %rax
movq %rbp, %rdi
call readlist
movq %rax, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq %r10, %rsi
call printlist
movq $0, %rax
movq -24(%rbp), %r12
jmp L130
L130:
jmp final
printlist:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, %r10
movq %r10, -16(%rbp)
movq $0, %r11
movq -16(%rbp), %r10
cmp %r10, %r11
je L113
jmp L114
L114:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r11
movq -16(%rbp), %r10
xorq %rax, %rax
movq %r11, %rdi
movq $0, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, %rsi
call printint
xorq %rax, %rax
movq $L109, %r10
movq %r10, %rdi
call print
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r11
movq -16(%rbp), %r10
xorq %rax, %rax
movq %r11, %rdi
movq $1, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, %rsi
call printlist
L115:
jmp L135
L113:
xorq %rax, %rax
movq $L105, %r10
movq %r10, %rdi
call print
jmp L115
L135:
movq %rbp, %rsp
popq %rbp
ret
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
jl L96
jmp L97
L97:
movq $0, %r10
movq -16(%rbp), %rsi
cmp %r10, %rsi
jg L93
jmp L94
L94:
xorq %rax, %rax
movq $L91, %r10
movq %r10, %rdi
call print
L95:
L98:
jmp L154
L96:
xorq %rax, %rax
movq $L87, %r10
movq %r10, %rdi
call print
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L98
L93:
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L95
L154:
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
jg L80
jmp L81
L81:
jmp L161
L80:
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
movq $L76, %r10
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
jmp L81
L161:
movq %rbp, %rsp
popq %rbp
ret
readlist:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-32, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
xorq %rax, %rax
movq $1, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _allocRecord
movq %rax, %r10
movq %r10, -16(%rbp)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq -16(%rbp), %r10
movq %r10, %rsi
call readint
movq %rax, %r10
movq %r10, -24(%rbp)
movq -16(%rbp), %r10
movq $0, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r11
movq $0, %r10
cmp %r11, %r10
jne L64
jmp L65
L65:
movq $0, %rax
L66:
jmp L177
L64:
movq $2, %r10
movq %r10, -32(%rbp)
movq -24(%rbp), %r10
movq %r10, -40(%rbp)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
call readlist
movq %rax, %r11
xorq %rax, %rax
movq -32(%rbp), %r10
movq %r10, %rdi
movq -40(%rbp), %r10
movq %r10, %rsi
movq %r11, %rdx
call _allocRecord
jmp L66
L177:
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
jmp L195
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
L195:
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
jmp L225
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
L225:
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
jmp L251
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
L251:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
