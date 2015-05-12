.section	.rodatan
	.align 4
.text
.globl tigermain
	.type	tigermain,@function
tigermain: 
	# Standard function prologue
	pushl %ebp
	movl %esp, %ebp
	subl $88, %esp
L2:
	movl $3, %edx
	movl $5, %ecx
	addl %ecx, %edx
	jmp L1
L1:
	# Standard function epilogue
	movl %ebp, %esp
	popl %ebp
	ret
