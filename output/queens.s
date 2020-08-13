.data
L29:
  .quad 1
 .string "
"
  L22:
  .quad 1
 .string "
"
  L11:
  .quad 2
 .string " ."
  L10:
  .quad 2
 .string " O"
  
.text
.globl tigermain
tigermain:
pushq %rbp
movq %rsp, %rbp
subq $56, %rsp
addq $-8, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %r12, -56(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq $8, %r11
movq %r11, (%r10)
movq %rbp, %r12
addq $-24, %r12
xorq %rax, %rax
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, (%r12)
movq %rbp, %r12
addq $-32, %r12
xorq %rax, %rax
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, (%r12)
movq %rbp, %r12
addq $-40, %r12
xorq %rax, %rax
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %rbp, %r11
addq $-16, %r11
movq (%r11), %r11
addq %r11, %r10
movq $1, %r11
subq %r11, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, (%r12)
movq %rbp, %r12
addq $-48, %r12
xorq %rax, %rax
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %rbp, %r11
addq $-16, %r11
movq (%r11), %r11
addq %r11, %r10
movq $1, %r11
subq %r11, %r10
movq %r10, %rdi
movq $0, %r10
movq %r10, %rsi
call _initArray
movq %rax, (%r12)
xorq %rax, %rax
movq %rbp, %rdi
movq $0, %r10
movq %r10, %rsi
call try
movq $0, %rax
movq -56(%rbp), %r12
jmp L103
L103:
jmp final
try:
pushq %rbp
movq %rsp, %rbp
subq $24, %rsp
addq $-192, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rsi, -24(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq -24(%rbp), %rsi
cmp %rsi, %r10
je L81
jmp L82
L82:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq $1, %r11
subq %r11, %r10
movq %r10, -32(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -32(%rbp), %r10
cmp %r10, %r11
jle L79
jmp L38
L38:
L83:
jmp L134
L81:
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
call printboard
jmp L83
L79:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-24, %r10
movq (%r10), %r11
movq %r11, -40(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, -48(%rbp)
xorq %rax, %rax
movq -40(%rbp), %r11
movq %r11, %rdi
movq -48(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -48(%rbp), %r10
imul %r11, %r10
movq -40(%rbp), %r11
addq %r10, %r11
movq (%r11), %r11
movq $0, %r10
cmp %r11, %r10
je L46
jmp L47
L47:
movq $0, %r10
L48:
movq $0, %r11
cmp %r10, %r11
jne L55
jmp L56
L56:
movq $0, %r10
L57:
movq $0, %r11
cmp %r10, %r11
jne L74
jmp L75
L75:
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -32(%rbp), %r10
cmp %r11, %r10
je L38
jmp L80
L80:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L79
L46:
movq $1, %r10
movq %r10, -56(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-40, %r10
movq (%r10), %r11
movq %r11, -64(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq -24(%rbp), %rsi
addq %rsi, %r10
movq %r10, -72(%rbp)
xorq %rax, %rax
movq -64(%rbp), %r11
movq %r11, %rdi
movq -72(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -72(%rbp), %r10
imul %r11, %r10
movq -64(%rbp), %r11
addq %r10, %r11
movq (%r11), %r11
movq $0, %r10
cmp %r11, %r10
je L44
jmp L45
L45:
movq $0, %r10
movq %r10, -56(%rbp)
L44:
movq -56(%rbp), %r10
jmp L48
L55:
movq $1, %r10
movq %r10, -80(%rbp)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-48, %r10
movq (%r10), %r11
movq %r11, -88(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $7, %r10
movq -24(%rbp), %rsi
subq %rsi, %r10
movq %r10, -96(%rbp)
xorq %rax, %rax
movq -88(%rbp), %r11
movq %r11, %rdi
movq -96(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -96(%rbp), %r10
imul %r11, %r10
movq -88(%rbp), %r11
addq %r10, %r11
movq (%r11), %r11
movq $0, %r10
cmp %r11, %r10
je L53
jmp L54
L54:
movq $0, %r10
movq %r10, -80(%rbp)
L53:
movq -80(%rbp), %r10
jmp L57
L74:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-24, %r10
movq (%r10), %r11
movq %r11, -104(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, -112(%rbp)
xorq %rax, %rax
movq -104(%rbp), %r11
movq %r11, %rdi
movq -112(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -112(%rbp), %r10
imul %r11, %r10
movq -104(%rbp), %r11
addq %r10, %r11
movq $1, %r10
movq %r10, (%r11)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-40, %r10
movq (%r10), %r11
movq %r11, -120(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq -24(%rbp), %rsi
addq %rsi, %r10
movq %r10, -128(%rbp)
xorq %rax, %rax
movq -120(%rbp), %r11
movq %r11, %rdi
movq -128(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -128(%rbp), %r10
imul %r11, %r10
movq -120(%rbp), %r11
addq %r10, %r11
movq $1, %r10
movq %r10, (%r11)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-48, %r10
movq (%r10), %r11
movq %r11, -136(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $7, %r10
movq -24(%rbp), %rsi
subq %rsi, %r10
movq %r10, -144(%rbp)
xorq %rax, %rax
movq -136(%rbp), %r11
movq %r11, %rdi
movq -144(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -144(%rbp), %r10
imul %r11, %r10
movq -136(%rbp), %r11
addq %r10, %r11
movq $1, %r10
movq %r10, (%r11)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-32, %r10
movq (%r10), %r11
movq %r11, -152(%rbp)
movq -24(%rbp), %rsi
movq %rsi, %r10
movq %r10, -160(%rbp)
xorq %rax, %rax
movq -152(%rbp), %r11
movq %r11, %rdi
movq -160(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -160(%rbp), %r10
imul %r11, %r10
movq -152(%rbp), %r11
addq %r10, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, (%r11)
xorq %rax, %rax
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
movq %r10, %rdi
movq -24(%rbp), %rsi
addq $1, %rsi
movq %rsi, %rsi
call try
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-24, %r10
movq (%r10), %r11
movq %r11, -168(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, -176(%rbp)
xorq %rax, %rax
movq -168(%rbp), %r11
movq %r11, %rdi
movq -176(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -176(%rbp), %r10
imul %r11, %r10
movq -168(%rbp), %r11
addq %r10, %r11
movq $0, %r10
movq %r10, (%r11)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-40, %r10
movq (%r10), %r11
movq %r11, -184(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq -24(%rbp), %rsi
addq %rsi, %r10
movq %r10, -192(%rbp)
xorq %rax, %rax
movq -184(%rbp), %r11
movq %r11, %rdi
movq -192(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -192(%rbp), %r10
imul %r11, %r10
movq -184(%rbp), %r11
addq %r10, %r11
movq $0, %r10
movq %r10, (%r11)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-48, %r10
movq (%r10), %r11
movq %r11, -200(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $7, %r10
movq -24(%rbp), %rsi
subq %rsi, %r10
movq %r10, -208(%rbp)
xorq %rax, %rax
movq -200(%rbp), %r11
movq %r11, %rdi
movq -208(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -208(%rbp), %r10
imul %r11, %r10
movq -200(%rbp), %r11
addq %r10, %r11
movq $0, %r10
movq %r10, (%r11)
jmp L75
L134:
movq %rbp, %rsp
popq %rbp
ret
printboard:
pushq %rbp
movq %rsp, %rbp
subq $32, %rsp
addq $-32, %rsp
movq %rbp, %r10
addq $-8, %r10
movq %rdi, (%r10)
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq $1, %r11
subq %r11, %r10
movq %r10, -32(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -32(%rbp), %r10
cmp %r10, %r11
jle L27
jmp L6
L6:
xorq %rax, %rax
movq $L29, %r10
movq %r10, %rdi
call print
jmp L274
L27:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-16, %r10
movq (%r10), %r10
movq $1, %r11
subq %r11, %r10
movq %r10, -40(%rbp)
movq %rbp, %r10
addq $-24, %r10
movq $0, %r11
movq %r11, (%r10)
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r11
movq -40(%rbp), %r10
cmp %r10, %r11
jle L20
jmp L7
L7:
xorq %rax, %rax
movq $L22, %r10
movq %r10, %rdi
call print
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r11
movq -32(%rbp), %r10
cmp %r11, %r10
je L6
jmp L28
L28:
movq %rbp, %r11
addq $-16, %r11
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L27
L20:
movq %rbp, %r10
addq $-8, %r10
movq (%r10), %r10
addq $-32, %r10
movq (%r10), %r11
movq %r11, -48(%rbp)
movq %rbp, %r10
addq $-16, %r10
movq (%r10), %r10
movq %r10, -56(%rbp)
xorq %rax, %rax
movq -48(%rbp), %r11
movq %r11, %rdi
movq -56(%rbp), %r10
movq %r10, %rsi
call _checkIndexArray
movq $8, %r11
movq -56(%rbp), %r10
imul %r11, %r10
movq -48(%rbp), %r11
addq %r10, %r11
movq (%r11), %r11
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
cmp %r11, %r10
je L12
jmp L13
L13:
movq $L11, %r10
L14:
xorq %rax, %rax
movq %r10, %rdi
call print
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r11
movq -40(%rbp), %r10
cmp %r11, %r10
je L7
jmp L21
L21:
movq %rbp, %r11
addq $-24, %r11
movq %rbp, %r10
addq $-24, %r10
movq (%r10), %r10
addq $1, %r10
movq %r10, (%r11)
jmp L20
L12:
movq $L10, %r10
jmp L14
L274:
movq %rbp, %rsp
popq %rbp
ret

final:
movq %rbp, %rsp
popq %rbp
ret
