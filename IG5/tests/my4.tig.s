L0:
.4byte 6
.ascii "euqla\n"
L1:
.4byte 2
.ascii "ab"
L2:
.4byte 3
.ascii "abc"
L3:
.4byte 2
.ascii "ab"
L4:
.4byte 3
.ascii "abc"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $2000,%esp
pushl %ebx
pushl %edi
pushl %esi
L9:
pushl %esp
pushl %esp
pushl %esp
leal L4,%eax
pushl %eax
leal L3,%eax
pushl %eax
call stringLessThanEqual
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl $1,%ebx
cmpl %ebx,%edi
je L5
L6:
movl $0,%eax
movl %eax,%ebx
L7:
movl %ebx,%eax
jmp L8
L5:
pushl %esp
pushl %esp
pushl %esp
leal L0,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L7
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
