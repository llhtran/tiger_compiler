.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L2:
movl $3,%eax
movl %eax,%eax
jmp L1
L1:
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
L4:
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L0
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L3
L3:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
