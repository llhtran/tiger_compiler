L1:
.4byte 4
.ascii "c=3\n"
L5:
.4byte 4
.ascii "yes\n"
L10:
.4byte 5
.ascii "Lien\n"
.globl L0
.type L0, @function
L0:
pushl %ebp
movl %esp,%ebp
subl $108,%esp
pushl %ebx
pushl %edi
pushl %esi
L17:
movl $3,%edi
movl 12(%ebp),%ebx
cmpl %edi,%ebx
je L2
L3:
movl $0,%ebx
movl %ebx,%ebx
L4:
movl $5,%edi
movl 8(%ebp),%ebx
movl -104(%ebx),%ebx
cmpl %edi,%ebx
je L6
L7:
movl $0,%eax
movl %eax,%eax
L8:
movl 12(%ebp),%eax
movl %eax,-104(%ebp)
movl 8(%ebp),%eax
movl -104(%eax),%eax
movl %eax,-108(%ebp)
movl -108(%ebp),%ebx
movl -104(%ebp),%eax
cmpl %ebx,%eax
jle L11
L9:
movl $0,%eax
movl %eax,%eax
jmp L16
L2:
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
jmp L4
L6:
pushl %eax
pushl %ecx
pushl %edx
leal L5,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L8
L11:
pushl %eax
pushl %ecx
pushl %edx
leal L10,%eax
pushl %eax
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
movl -104(%ebp),%ebx
movl $1,%eax
movl %ebx,%ebx
addl %eax,%ebx
movl %ebx,-104(%ebp)
movl -108(%ebp),%ebx
movl -104(%ebp),%eax
cmpl %ebx,%eax
jle L11
L18:
jmp L9
L16:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L12:
.4byte 4
.ascii "N=5\n"
.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L20:
movl $5,%ebx
movl %ebx,-104(%ebp)
movl $5,%edi
movl -104(%ebp),%ebx
cmpl %edi,%ebx
je L13
L14:
movl $0,%ebx
movl %ebx,%ebx
L15:
pushl %eax
pushl %ecx
pushl %edx
movl $3,%eax
pushl %eax
pushl %ebp
call L0
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L19
L13:
pushl %eax
pushl %ecx
pushl %edx
leal L12,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
jmp L15
L19:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
