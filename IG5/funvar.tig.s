L1:
.4byte 5
.ascii "Lien\n"
.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
movl $5,%ebx
movl %ebx,-2004(%ebp)
movl $5,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
je L2
L3:
movl $0,%eax
movl %eax,%ebx
L4:
movl %ebx,%eax
jmp L5
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
jmp L4
L5:
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
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L8:
pushl %esp
pushl %esp
pushl %esp
pushl %ebp
call L0
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L7
L7:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
