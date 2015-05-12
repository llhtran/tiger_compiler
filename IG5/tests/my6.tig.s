L0:
.4byte 4
.ascii "fail"
L1:
.4byte 8
.ascii "success\n"
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
movl %ebx,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%eax
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
movl $1,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -104(%ebp),%esi
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
movl $5,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -104(%ebp),%esi
movl $3,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
movl -104(%ebp),%esi
movl $3,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -104(%ebp),%esi
movl $0,%edi
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
