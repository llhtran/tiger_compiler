L0:
.4byte 2
.ascii "yo"
L1:
.4byte 3
.ascii "die"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
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
pushl %eax
pushl %ecx
pushl %edx
movl $2,%ebx
pushl %ebx
movl -104(%ebp),%ebx
pushl %ebx
call arrayBounds
addl $8,%esp
popl %edx
popl %ecx
popl %eax
movl $0,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -104(%ebp),%esi
movl $2,%edi
movl $1,%ebx
movl %edi,%edi
addl %ebx,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%ebx
je L2
L3:
pushl %eax
pushl %ecx
pushl %edx
leal L1,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L4:
movl %ebx,%eax
jmp L5
L2:
pushl %eax
pushl %ecx
pushl %edx
leal L0,%eax
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
