.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L12:
L8:
movl 12(%ebp),%ebx
movl %ebx,%edi
L9:
movl $0,%ebx
cmpl %ebx,%edi
je L5
L6:
L11:
movl 8(%ebp),%ebx
movl %ebx,%edi
L4:
L2:
movl 12(%ebp),%ebx
movl %ebx,%ebx
L3:
pushl %eax
pushl %ecx
pushl %edx
movl $1,%eax
movl %ebx,%ebx
subl %eax,%ebx
pushl %ebx
pushl %edi
call L1
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
L7:
movl %eax,%eax
jmp L26
L5:
L10:
movl $1,%eax
movl %eax,%eax
jmp L7
L26:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L23:
L19:
movl 12(%ebp),%ebx
movl %ebx,%edi
L20:
movl $0,%ebx
cmpl %ebx,%edi
je L16
L17:
L22:
movl 8(%ebp),%ebx
movl %ebx,%edi
L15:
L13:
movl 12(%ebp),%ebx
movl %ebx,%ebx
L14:
pushl %eax
pushl %ecx
pushl %edx
movl $1,%eax
movl %ebx,%ebx
subl %eax,%ebx
pushl %ebx
pushl %edi
call L0
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
L18:
movl %eax,%eax
jmp L27
L16:
L21:
movl $0,%eax
movl %eax,%eax
jmp L18
L27:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L25:
movl %ebp,%ebx
L24:
pushl %eax
pushl %ecx
pushl %edx
movl $3,%eax
pushl %eax
pushl %ebx
call L1
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L28
L28:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
