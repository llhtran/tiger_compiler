L1:
.4byte 3
.ascii "la\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2008,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
movl $0,%ebx
movl %ebx,-2004(%ebp)
movl $3,%ebx
movl %ebx,-2000(%ebp)
movl -2000(%ebp),%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
jle L5
L0:
movl $0,%eax
movl %eax,%eax
jmp L6
L5:
movl $2,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
je L2
L3:
jmp L0
L2:
pushl %esp
pushl %esp
pushl %esp
leal L1,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L4:
movl -2004(%ebp),%edi
movl $1,%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,-2004(%ebp)
movl -2000(%ebp),%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
jle L5
L9:
jmp L0
L8:
movl $0,%ebx
movl %ebx,%ebx
jmp L4
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
