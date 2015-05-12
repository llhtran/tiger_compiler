L1:
.4byte 8
.ascii "awesome\n"
L2:
.4byte 5
.ascii "fuck\n"
.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
movl $5,%edi
movl 8(%ebp),%ebx
movl -104(%ebx),%ebx
cmpl %edi,%ebx
je L3
L4:
pushl %eax
pushl %ecx
pushl %edx
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
jmp L6
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
jmp L5
L6:
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
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L9:
movl $5,%ebx
movl %ebx,-104(%ebp)
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L0
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L8
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
