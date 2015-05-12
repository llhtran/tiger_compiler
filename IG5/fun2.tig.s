L1:
.4byte 4
.ascii "Lien"
.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
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
movl -2004(%ebp),%eax
movl %eax,%eax
jmp L2
L2:
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
L5:
pushl %esp
pushl %esp
pushl %esp
movl $5,%eax
pushl %eax
pushl %ebp
call L0
addl $8,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L4
L4:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
