.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L5:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
movl $1,%eax
movl %ebx,%ebx
addl %eax,%ebx
pushl %ebx
movl 8(%ebp),%eax
pushl %eax
call L1
addl $8,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L4
L4:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L2:
.4byte 3
.ascii "str"
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
pushl %eax
pushl %ecx
pushl %edx
leal L2,%eax
pushl %eax
movl 12(%ebp),%eax
pushl %eax
movl 8(%ebp),%eax
pushl %eax
call L0
addl $12,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L6
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L3:
.4byte 4
.ascii "str2"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L9:
pushl %eax
pushl %ecx
pushl %edx
leal L3,%eax
pushl %eax
movl $0,%eax
pushl %eax
pushl %ebp
call L0
addl $12,%esp
popl %edx
popl %ecx
popl %eax
movl $1,%eax
movl %eax,%eax
jmp L8
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
