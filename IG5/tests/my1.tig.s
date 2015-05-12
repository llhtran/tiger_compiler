.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
movl 8(%ebp),%eax
movl 12(%eax),%ebx
movl 8(%ebp),%eax
movl 16(%eax),%eax
movl %ebx,%ebx
addl %eax,%ebx
movl 8(%ebp),%eax
movl 12(%eax),%eax
movl %ebx,%ebx
imull %eax,%ebx
movl %ebx,%eax
jmp L6
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L1:
.4byte 7
.ascii "amazing"
L2:
.4byte 3
.ascii "die"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L9:
movl $-2004,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
pushl %esp
pushl %esp
pushl %esp
movl $5,%eax
pushl %eax
movl $3,%eax
pushl %eax
pushl %ebp
call L0
addl $12,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl $24,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
je L3
L4:
pushl %esp
pushl %esp
pushl %esp
leal L2,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L5:
movl %ebx,%eax
jmp L8
L3:
pushl %esp
pushl %esp
pushl %esp
leal L1,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L5
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
