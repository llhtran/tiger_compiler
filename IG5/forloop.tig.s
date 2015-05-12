L1:
.4byte 5
.ascii "Lien\n"
L2:
.4byte 5
.ascii "Lien\n"
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
movl $1,%eax
movl %eax,-2004(%ebp)
movl $3,%eax
movl %eax,-2000(%ebp)
movl -2000(%ebp),%ebx
movl -2004(%ebp),%eax
cmpl %ebx,%eax
jle L5
L0:
movl $0,%eax
movl %eax,%eax
jmp L6
L5:
jmp L0
L8:
movl $0,%ebx
movl %ebx,%ebx
movl $3,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
je L3
L4:
pushl %esp
pushl %esp
pushl %esp
leal L2,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L3:
movl -2004(%ebp),%ebx
movl $1,%eax
movl %ebx,%ebx
addl %eax,%ebx
movl %ebx,-2004(%ebp)
movl -2000(%ebp),%ebx
movl -2004(%ebp),%eax
cmpl %ebx,%eax
jle L5
L9:
jmp L0
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
