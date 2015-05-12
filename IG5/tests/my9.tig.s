L1:
.4byte 5
.ascii "Lien\n"
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
leal L1,%eax
pushl %eax
call print
addl $4,%esp
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
.globl L3
.type L3, @function
L3:
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
movl 8(%ebp),%eax
movl 8(%eax),%eax
pushl %eax
call L0
addl $4,%esp
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
.globl L2
.type L2, @function
L2:
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
pushl %ebp
call L3
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L8
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L11:
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L2
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L10
L10:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
