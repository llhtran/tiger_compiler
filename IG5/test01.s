.section	.rodata
	.align 4
.text
.globl tigermain
	.type	tigermain,@function
tigermain: 
	# Standard function prologue
	pushl %ebp
	movl %esp, %ebp
	subl $96, %esp
L1:
	movl -96(%ebp), %eax
	jmp L0
L0:
	# Standard function epilogue
	movl %ebp, %esp
	popl %ebp
	ret
