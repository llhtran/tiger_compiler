.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L9:
movl $0,%edi
movl 12(%ebp),%ebx
cmpl %edi,%ebx
je L1
L2:
movl 12(%ebp),%ebx
movl %ebx,%edi
pushl %esp
pushl %esp
pushl %esp
movl 12(%ebp),%ebx
movl $1,%eax
movl %ebx,%ebx
subl %eax,%ebx
pushl %ebx
pushl %ebp
call L0
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %edi,%eax
imull %ebx,%eax
movl %eax,%eax
L3:
movl %eax,%eax
jmp L8
L1:
movl $1,%eax
movl %eax,%eax
jmp L3
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L4:
.4byte 8
.ascii "bitches\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L11:
movl $12,%ebx
movl %ebp,%edi
addl %ebx,%edi
movl %edi,%ebx
pushl %esp
pushl %esp
pushl %esp
movl $3,%eax
pushl %eax
pushl %ebp
call L0
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl $6,%edi
movl 12(%ebp),%ebx
cmpl %edi,%ebx
je L5
L6:
movl $0,%eax
movl %eax,%ebx
L7:
movl %ebx,%eax
jmp L10
L5:
pushl %esp
pushl %esp
pushl %esp
leal L4,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L7
L10:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
