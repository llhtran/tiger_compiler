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
movl $3,%ebx
movl $5,%eax
movl %ebx,%ebx
imull %eax,%ebx
movl %ebx,%eax
jmp L0
L0:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
