.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L1:
movl $1,%eax
movl %eax,%eax
jmp L0
L0:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
