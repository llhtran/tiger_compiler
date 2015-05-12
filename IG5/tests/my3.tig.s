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
pushl %ebx
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
movl 16(%ebp),%ebx
pushl %ebx
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
movl 20(%ebp),%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L4
L4:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L1:
.4byte 5
.ascii "Lien\n"
L2:
.4byte 5
.ascii "Tran\n"
L3:
.4byte 5
.ascii "Yale\n"
.globl tigermain
.type tigermain, @function
tigermain:
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
leal L3,%eax
pushl %eax
leal L2,%eax
pushl %eax
leal L1,%eax
pushl %eax
pushl %ebp
call L0
addl $16,%esp
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
