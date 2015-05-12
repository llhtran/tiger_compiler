.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L3:
movl $5,%eax
movl %eax,-2004(%ebp)
movl $10,%ebx
movl -2004(%ebp),%eax
cmpl %ebx,%eax
jl L1
L0:
movl $0,%eax
movl %eax,%eax
jmp L2
L1:
jmp L0
L4:
movl $10,%ebx
movl -2004(%ebp),%eax
cmpl %ebx,%eax
jl L1
L5:
jmp L0
L2:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
