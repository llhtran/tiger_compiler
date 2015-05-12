.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L8:
movl 8(%ebp),%eax
movl 12(%eax),%ebx
movl $11,%eax
movl %ebx,%ebx
addl %eax,%ebx
movl %ebx,%eax
jmp L7
L7:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L10:
movl $-2004,%edi
movl 8(%ebp),%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,%ebx
pushl %esp
pushl %esp
pushl %esp
movl 8(%ebp),%eax
movl 12(%eax),%eax
pushl %eax
movl 8(%ebp),%eax
pushl %eax
call L0
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl 8(%ebp),%eax
movl -2004(%eax),%eax
movl %eax,%eax
jmp L9
L9:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L2:
.4byte 8
.ascii "success\n"
L3:
.4byte 5
.ascii "fail\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2004,%esp
pushl %ebx
pushl %edi
pushl %esi
L12:
movl $0,%ebx
movl %ebx,-2004(%ebp)
movl $-2004,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
pushl %esp
pushl %esp
pushl %esp
movl $3,%eax
pushl %eax
pushl %ebp
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl $14,%edi
movl -2004(%ebp),%ebx
cmpl %edi,%ebx
je L4
L5:
pushl %esp
pushl %esp
pushl %esp
leal L3,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L6:
movl %ebx,%eax
jmp L11
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
jmp L6
L11:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
