.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
movl 12(%ebp),%ebx
movl $3,%eax
movl %ebx,%ebx
addl %eax,%ebx
movl %ebx,%eax
jmp L5
L5:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L1:
.4byte 5
.ascii "yaya\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L8:
movl $12,%ebx
movl %ebp,%edi
addl %ebx,%edi
movl %edi,%ebx
pushl %esp
pushl %esp
pushl %esp
movl $3,%eax
pushl %eax
pushl %ebp
call L0
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl $5,%edi
movl 12(%ebp),%ebx
cmpl %edi,%ebx
jg L2
L3:
movl $0,%eax
movl %eax,%ebx
L4:
movl %ebx,%eax
jmp L7
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
L7:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
