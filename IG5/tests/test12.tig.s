L1:
.4byte 3
.ascii "hi\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2012,%esp
pushl %ebx
pushl %edi
pushl %esi
L4:
movl $0,%ebx
movl %ebx,-2004(%ebp)
movl $0,%ebx
movl %ebx,-2000(%ebp)
movl $10,%ebx
movl %ebx,-1996(%ebp)
movl -1996(%ebp),%edi
movl -2000(%ebp),%ebx
cmpl %edi,%ebx
jle L2
L0:
movl $0,%eax
movl %eax,%eax
jmp L3
L2:
movl -2004(%ebp),%edi
movl $1,%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,-2004(%ebp)
pushl %esp
pushl %esp
pushl %esp
leal L1,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl -2000(%ebp),%edi
movl $1,%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,-2000(%ebp)
movl -1996(%ebp),%edi
movl -2000(%ebp),%ebx
cmpl %edi,%ebx
jle L2
L5:
jmp L0
L3:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
