.data
L136:
  .quad 1
 .string " "
  L132:
  .quad 1
 .string "
"
  L118:
  .quad 1
 .string "0"
  L114:
  .quad 1
 .string "-"
  L103:
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
addq $-16, %rsp
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
movq %r10, -32(%rbp)
movq %rbp, %r12
addq $-16, %r12
xorq %rax, %rax
call getstr
movq %rax, (%r12)
xorq %rax, %rax
movq %rbp, %rdi
call readlist
movq %rax, %r11
xorq %rax, %rax
movq %rbp, %rdi
movq -32(%rbp), %r10
movq %r10, %rsi
movq %r11, %rdx
call merge
movq %rax, %r10
xorq %rax, %rax
movq %rbp, %rdi
movq %r10, %rsi
call printlist
movq $0, %rax
movq -24(%rbp), %r12
jmp L163
L163:
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
je L140
jmp L141
L141:
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
movq $L136, %r10
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
L142:
jmp L169
L140:
xorq %rax, %rax
movq $L132, %r10
movq %r10, %rdi
call print
jmp L142
L169:
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
jl L123
jmp L124
L124:
movq $0, %r10
movq -16(%rbp), %rsi
cmp %r10, %rsi
jg L120
jmp L121
L121:
xorq %rax, %rax
movq $L118, %r10
movq %r10, %rdi
call print
L122:
L125:
jmp L188
L123:
xorq %rax, %rax
movq $L114, %r10
movq %r10, %rdi
call print
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L125
L120:
xorq %rax, %rax
movq %rbp, %rdi
movq -16(%rbp), %rsi
movq %rsi, %rsi
call f
jmp L122
L188:
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
jg L107
jmp L108
L108:
jmp L195
L107:
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
movq $L103, %r10
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
jmp L108
L195:
movq %rbp, %rsp
popq %rbp
ret
merge:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
addq $-48, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, %r11
movq %r11, -16(%rbp)
movq %rdx, %r10
movq %r10, -24(%rbp)
movq $0, %r10
movq -16(%rbp), %r11
cmp %r11, %r10
je L91
jmp L92
L92:
movq $0, %r11
movq -24(%rbp), %r10
cmp %r10, %r11
je L87
jmp L88
L88:
movq -16(%rbp), %r11
movq $0, %r10
movq $8, %r8
imul %r8, %r10
addq %r10, %r11
movq (%r11), %r8
movq -24(%rbp), %r10
movq $0, %r11
movq $8, %r9
imul %r9, %r11
addq %r11, %r10
movq (%r10), %r10
cmp %r10, %r8
jl L83
jmp L84
L84:
movq $2, %r10
movq %r10, -32(%rbp)
movq -24(%rbp), %r10
movq $0, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, -40(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r8
movq -16(%rbp), %r11
movq -24(%rbp), %r10
xorq %rax, %rax
movq %r8, %rdi
movq %r11, %rsi
movq $1, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, %rdx
call merge
movq %rax, %r11
xorq %rax, %rax
movq -32(%rbp), %r10
movq %r10, %rdi
movq -40(%rbp), %r10
movq %r10, %rsi
movq %r11, %rdx
call _allocRecord
L85:
movq %rax, %r11
L89:
movq %r11, %r10
L93:
movq %r10, %rax
jmp L217
L91:
movq -24(%rbp), %r10
jmp L93
L87:
movq -16(%rbp), %r11
jmp L89
L83:
movq $2, %r10
movq %r10, -48(%rbp)
movq -16(%rbp), %r11
movq %r11, %r10
movq $0, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, -56(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r8
movq -16(%rbp), %r11
movq %r11, %r10
xorq %rax, %rax
movq %r8, %rdi
movq $1, %r11
movq $8, %r8
imul %r8, %r11
addq %r11, %r10
movq (%r10), %r10
movq %r10, %rsi
movq -24(%rbp), %r10
movq %r10, %rdx
call merge
movq %rax, %r11
xorq %rax, %rax
movq -48(%rbp), %r10
movq %r10, %rdi
movq -56(%rbp), %r10
movq %r10, %rsi
movq %r11, %rdx
call _allocRecord
jmp L85
L217:
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
jmp L257
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
L257:
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
jmp L275
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
L275:
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
jmp L305
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
L305:
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
jmp L331
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
L331:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
