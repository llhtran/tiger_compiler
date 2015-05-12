.section	.rodata
	.align 4
.text
.globl tigermain
	.type	tigermain,@function
tigermain: 
	# Standard function prologue
	pushl %ebp
	movl %esp, %ebp
	subl $92, %esp
L3:
	movl $1, %eax
	movl %eax, %edx
	movl $1, %ecx
	movl $3, %eax
	cmp %ecx, %eax
	je L0
L1:
	movl $0, %eax
	movl %eax, %edx
L0:
	movl %edx, %eax
	jmp L2
L2:
	# Standard function epilogue
	movl %ebp, %esp
	popl %ebp
	ret
