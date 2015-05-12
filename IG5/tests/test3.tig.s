L0:
.4byte 6
.ascii "Nobody"
L1:
.4byte 6
.ascii "Nobody"
L9:
.4byte 8
.ascii "Somebody"
L13:
.4byte 8
.ascii "Somebody"
L20:
.4byte 9
.ascii "amamazing"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L41:
L4:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%esi
L5:
pushl %eax
pushl %ecx
pushl %edx
movl $8,%eax
pushl %eax
call allocRecord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl $0,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %ebx,%edi
movl %edi,%edi
L2:
leal L1,%ebx
movl %ebx,(%edi)
movl $4,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %ebx,%edi
movl %edi,%edi
L3:
movl $300,%ebx
movl %ebx,(%edi)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%esi)
L40:
L14:
L10:
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L11:
movl -104(%ebp),%ebx
movl %ebx,%esi
L12:
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl %ebx,%edi
L15:
leal L13,%ebx
movl %ebx,(%edi)
L39:
L19:
L16:
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L17:
movl -104(%ebp),%ebx
movl %ebx,%esi
L18:
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
pushl %ebx
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L38:
L34:
L28:
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L29:
movl -104(%ebp),%ebx
movl %ebx,%esi
L30:
movl $1,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
movl %ebx,%edi
L35:
movl $300,%ebx
cmpl %ebx,%edi
je L31
L32:
L37:
movl $0,%eax
movl %eax,%eax
L33:
movl %eax,%eax
jmp L42
L31:
L36:
L21:
pushl %eax
pushl %ecx
pushl %edx
leal L20,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L33
L42:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
