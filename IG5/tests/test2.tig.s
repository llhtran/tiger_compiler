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
movl %ebx,%edi
pushl %eax
pushl %ecx
pushl %edx
movl $0,%eax
pushl %eax
movl $1,%ebx
movl $10,%eax
movl %ebx,%ebx
addl %eax,%ebx
pushl %ebx
call initArray
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,(%edi)
movl -104(%ebp),%eax
movl $1,%eax
movl %eax,%eax
jmp L0
L0:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
