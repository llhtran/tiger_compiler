L0:
.4byte 6
.ascii "Nobody"
L1:
.4byte 6
.ascii "Nobody"
L2:
.4byte 8
.ascii "Somebody"
L3:
.4byte 8
.ascii "Somebody"
L4:
.4byte 3
.ascii "HI\n"
L5:
.4byte 4
.ascii "Bye\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L10:
movl $-2004,%ebx
movl %ebp,%edi
addl %ebx,%edi
movl %edi,%edi
pushl %esp
pushl %esp
pushl %esp
movl $8,%eax
pushl %eax
call allocRecord
addl $4,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
leal L1,%ebx
movl %ebx,0(%esi)
movl $1000,%ebx
movl %ebx,4(%esi)
movl %esi,(%edi)
pushl %esp
pushl %esp
pushl %esp
movl -2004(%ebp),%eax
pushl %eax
call recnilcheck
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
leal L3,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -2004(%ebp),%esi
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
pushl %esp
pushl %esp
pushl %esp
movl -2004(%ebp),%eax
pushl %eax
call recnilcheck
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl $500,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -2004(%ebp),%esi
movl $1,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
pushl %esp
pushl %esp
pushl %esp
movl -2004(%ebp),%eax
pushl %eax
call recnilcheck
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl $1000,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -2004(%ebp),%esi
movl $1,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%ebx
je L6
L7:
pushl %esp
pushl %esp
pushl %esp
leal L5,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L8:
movl %ebx,%eax
jmp L9
L6:
pushl %esp
pushl %esp
pushl %esp
leal L4,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L8
L9:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
