.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L1:
movl $-2004,%ebx
movl %ebp,%edi
addl %ebx,%edi
movl %edi,%ebx
pushl %esp
pushl %esp
pushl %esp
movl $7,%eax
pushl %eax
movl $5,%eax
pushl %eax
call initArray
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl -2004(%ebp),%edi
movl $0,%ebx
movl $4,%eax
movl %ebx,%ebx
imull %eax,%ebx
movl %edi,%eax
addl %ebx,%eax
movl (%eax),%eax
movl %eax,%eax
jmp L0
L0:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
