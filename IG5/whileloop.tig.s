L0:
.4byte 5
.ascii "Lien\n"
L1:
.4byte 5
.ascii "Lien\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L5:
movl $5,%ebx
movl %ebx,-2004(%ebp)
movl $10,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
jl L3
L2:
movl $0,%eax
movl %eax,%eax
jmp L4
L3:
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
movl -2004(%ebp),%edi
movl $1,%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,-2004(%ebp)
movl $10,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
jl L3
L6:
jmp L2
L4:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
