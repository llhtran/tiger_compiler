L0:
.4byte 12
.ascii "Hello World\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L2:
leal L0,%ebx
movl %ebx,-2004(%ebp)
pushl %esp
pushl %esp
pushl %esp
movl -2004(%ebp),%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L1
L1:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
