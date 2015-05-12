.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L1:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $8,%eax
pushl %eax
call allocRecord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl $5,%eax
movl %eax,0(%edi)
movl $0,%eax
movl %eax,4(%edi)
movl %edi,(%ebx)
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%eax
pushl %eax
call recnilcheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
movl -104(%ebp),%edi
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
