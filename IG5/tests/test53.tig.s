L0:
.4byte 2
.ascii "5\n"
L1:
.4byte 2
.ascii "6\n"
L5:
.4byte 2
.ascii "7\n"
L6:
.4byte 2
.ascii "8\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L14:
movl $0,%edi
movl $1,%ebx
cmpl %edi,%ebx
je L10
L11:
movl $0,%edi
movl $1,%ebx
cmpl %edi,%ebx
jne L7
L8:
pushl %eax
pushl %ecx
pushl %edx
leal L6,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L9:
movl %ebx,%eax
L12:
movl $1,%eax
movl %eax,%eax
jmp L13
L10:
movl $1,%edi
movl $1,%ebx
cmpl %edi,%ebx
je L2
L3:
pushl %eax
pushl %ecx
pushl %edx
leal L1,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
L4:
movl %ebx,%eax
jmp L12
L2:
pushl %eax
pushl %ecx
pushl %edx
leal L0,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L4
L7:
pushl %eax
pushl %ecx
pushl %edx
leal L5,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L9
L13:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
