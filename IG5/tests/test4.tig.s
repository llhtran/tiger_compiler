.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L16:
L12:
movl 12(%ebp),%ebx
movl %ebx,%edi
L13:
movl $0,%ebx
cmpl %ebx,%edi
je L9
L10:
L15:
L7:
movl 12(%ebp),%ebx
movl %ebx,%edi
L8:
movl 8(%ebp),%ebx
movl %ebx,%esi
L6:
L4:
movl 12(%ebp),%ebx
movl %ebx,%ebx
L5:
pushl %eax
pushl %ecx
pushl %edx
movl $1,%eax
movl %ebx,%ebx
subl %eax,%ebx
pushl %ebx
pushl %esi
call L0
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %edi,%eax
imull %ebx,%eax
movl %eax,%eax
L11:
movl %eax,%eax
jmp L19
L9:
L14:
movl $1,%eax
movl %eax,%eax
jmp L11
L19:
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
L18:
movl %ebp,%ebx
L17:
pushl %eax
pushl %ecx
pushl %edx
movl $4,%eax
pushl %eax
pushl %ebx
call L0
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L20
L20:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
