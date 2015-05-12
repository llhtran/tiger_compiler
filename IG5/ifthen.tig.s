L0:
.4byte 3
.ascii "YES"
L1:
.4byte 2
.ascii "NO"
L2:
.4byte 3
.ascii "YES"
L3:
.4byte 2
.ascii "NO"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2008,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
movl $3,%ebx
movl %ebx,-2004(%ebp)
movl $3,%ebx
movl %ebx,-2000(%ebp)
pushl %esp
pushl %esp
pushl %esp
leal L2,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl -2000(%ebp),%edi
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
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
L4:
movl $0,%eax
movl %eax,%eax
jmp L6
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
