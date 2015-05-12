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
L1:
	movl $1, %eax
	movl %eax, %eax
	jmp L0
L0:
	# Standard function epilogue
	movl %ebp, %esp
	popl %ebp
	ret
