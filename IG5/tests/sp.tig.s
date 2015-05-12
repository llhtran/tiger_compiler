.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L1:
movl $1,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl $2,%ecx
	movl	%ecx, -80(%ebp) # save pseudo-register
movl $3,%ecx
	movl	%ecx, -76(%ebp) # save pseudo-register
movl $4,%ecx
	movl	%ecx, -72(%ebp) # save pseudo-register
movl $5,%ecx
	movl	%ecx, -68(%ebp) # save pseudo-register
movl $6,%ecx
	movl	%ecx, -64(%ebp) # save pseudo-register
movl $7,%ecx
	movl	%ecx, -60(%ebp) # save pseudo-register
movl $8,%ecx
	movl	%ecx, -56(%ebp) # save pseudo-register
movl $9,%ecx
	movl	%ecx, -52(%ebp) # save pseudo-register
movl $10,%ecx
	movl	%ecx, -48(%ebp) # save pseudo-register
movl $11,%ecx
	movl	%ecx, -44(%ebp) # save pseudo-register
movl $12,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl $13,%esi
movl $14,%edi
movl $15,%ebx
movl $16,%eax
movl %ebx,%ebx
addl %eax,%ebx
movl %edi,%eax
addl %ebx,%eax
movl %esi,%ebx
addl %eax,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-44(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
	movl	-48(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-52(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
	movl	-56(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-60(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
	movl	-64(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-68(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
	movl	-72(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-76(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
	movl	-80(%ebp), %ecx # load pseudo-register
movl %ecx,%eax
addl %ebx,%eax
	movl	-12(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %eax,%ebx
movl %ebx,%eax
jmp L0
L0:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
