L8:
.4byte 1
.ascii "9"
L11:
.4byte 1
.ascii "9"
L14:
.4byte 1
.ascii "0"
L17:
.4byte 1
.ascii "0"
.globl L5
.type L5, @function
L5:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L29:
L22:
L16:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
movl 8(%eax),%eax
movl -104(%eax),%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%ebx
L23:
L18:
pushl %eax
pushl %ecx
pushl %edx
leal L17,%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
cmpl %edi,%ebx
jge L19
L20:
L28:
movl $0,%eax
movl %eax,%eax
L21:
movl %eax,%eax
jmp L240
L19:
movl $1,%ebx
movl %ebx,%esi
L26:
L10:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
movl 8(%eax),%eax
movl -104(%eax),%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%ebx
L27:
L12:
pushl %eax
pushl %ecx
pushl %edx
leal L11,%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
cmpl %edi,%ebx
jle L24
L25:
movl $0,%eax
movl %eax,%esi
L24:
movl %esi,%eax
jmp L21
L240:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L31:
.4byte 1
.ascii "\n"
L32:
.4byte 1
.ascii "\n"
L33:
.4byte 1
.ascii "\n"
L34:
.4byte 1
.ascii " "
L35:
.4byte 1
.ascii " "
L36:
.4byte 1
.ascii " "
.globl L6
.type L6, @function
L6:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L40:
movl 8(%ebp),%ebx
movl 8(%ebx),%ebx
movl -104(%ebx),%ebx
movl %ebx,%ebx
L41:
pushl %eax
pushl %ecx
pushl %edx
leal L36,%eax
pushl %eax
pushl %ebx
call stringEqual
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl $1,%ebx
cmpl %ebx,%edi
je L37
L38:
movl $1,%ebx
movl %ebx,%esi
L45:
movl 8(%ebp),%ebx
movl 8(%ebx),%ebx
movl -104(%ebx),%ebx
movl %ebx,%ebx
L46:
pushl %eax
pushl %ecx
pushl %edx
leal L33,%eax
pushl %eax
pushl %ebx
call stringEqual
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl $1,%ebx
cmpl %ebx,%edi
je L43
L44:
movl $0,%ebx
movl %ebx,%esi
L43:
movl %esi,%ebx
L39:
movl %ebx,%edi
movl $0,%ebx
cmpl %edi,%ebx
jne L49
L30:
movl $0,%eax
movl %eax,%eax
jmp L241
L37:
L42:
movl $1,%ebx
movl %ebx,%ebx
jmp L39
L49:
L47:
movl $-104,%edi
movl 8(%ebp),%ebx
movl 8(%ebx),%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,%ebx
L48:
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
jmp L40
L241:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L70:
.4byte 1
.ascii "0"
L80:
.4byte 1
.ascii "0"
L92:
.4byte 1
.ascii "0"
L102:
.4byte 1
.ascii "0"
.globl L2
.type L2, @function
L2:
pushl %ebp
movl %esp,%ebp
subl $104,%esp
pushl %ebx
pushl %edi
pushl %esi
L115:
L3:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%edi
L4:
movl $0,%ebx
movl %ebx,(%edi)
L114:
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L6
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L113:
L58:
L54:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L55:
movl 12(%ebp),%ebx
movl %ebx,%esi
L56:
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl %ebx,%ebx
L59:
movl %ebp,%edi
L57:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
movl -104(%eax),%eax
pushl %eax
pushl %edi
call L5
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
L112:
movl %ebp,%esi
L61:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
movl -104(%eax),%eax
pushl %eax
pushl %esi
call L5
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl $0,%ebx
cmpl %edi,%ebx
jne L110
L60:
L111:
movl -104(%ebp),%eax
movl %eax,%eax
jmp L242
L110:
L106:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
L107:
L104:
L100:
L97:
movl -104(%ebp),%ebx
movl %ebx,%edi
L98:
movl $10,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %edi,%ebx
L101:
L99:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
movl -104(%eax),%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %ebx,%ebx
addl %edi,%ebx
movl %ebx,%ebx
L105:
L103:
pushl %eax
pushl %ecx
pushl %edx
leal L102,%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %ebx,%ebx
subl %edi,%ebx
	movl	-8(%ebp), %edx # load pseudo-register
movl %ebx,(%edx)
L108:
movl $-104,%edi
movl 8(%ebp),%ebx
movl %edi,%edi
addl %ebx,%edi
movl %edi,%ebx
L109:
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl %ebp,%ebx
jmp L61
L242:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L116
.type L116, @function
L116:
pushl %ebp
movl %esp,%ebp
subl $108,%esp
pushl %ebx
pushl %edi
pushl %esi
L136:
L120:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%esi
L121:
pushl %eax
pushl %ecx
pushl %edx
movl $4,%eax
pushl %eax
call allocRecord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl $0,%ebx
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %ebx,%edi
movl %edi,%edi
L119:
movl $0,%ebx
movl %ebx,(%edi)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%esi)
L123:
movl $-108,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%edi
L124:
movl 8(%ebp),%ebx
movl %ebx,%ebx
L122:
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%eax
pushl %eax
pushl %ebx
call L2
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,(%edi)
L135:
L127:
pushl %eax
pushl %ecx
pushl %edx
movl -104(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L128:
movl -104(%ebp),%ebx
movl %ebx,%esi
L129:
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
movl %ebx,%edi
movl $0,%ebx
cmpl %edi,%ebx
jne L130
L131:
L134:
movl $0,%eax
movl %eax,%eax
L132:
movl %eax,%eax
jmp L243
L130:
L133:
pushl %eax
pushl %ecx
pushl %edx
movl $8,%eax
pushl %eax
call allocRecord
addl $4,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl $0,%ebx
movl %esi,%edi
addl %ebx,%edi
movl %edi,%edi
L125:
movl $-108,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl (%ebx),%ebx
movl %ebx,(%edi)
movl $4,%ebx
movl %esi,%edi
addl %ebx,%edi
movl %edi,%ebx
L126:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%eax
pushl %eax
call L116
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
movl %esi,%eax
jmp L132
L243:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L155:
.4byte 1
.ascii "0"
L171:
.4byte 1
.ascii "0"
.globl L137
.type L137, @function
L137:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L185:
L181:
movl 12(%ebp),%eax
movl %eax,%ebx
L182:
movl $0,%eax
cmpl %eax,%ebx
jg L178
L179:
L184:
movl $0,%eax
movl %eax,%eax
L180:
movl %eax,%eax
jmp L244
L178:
L183:
movl 8(%ebp),%eax
movl %eax,%edi
L140:
L138:
movl 12(%ebp),%eax
movl %eax,%eax
L139:
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edx
movl %eax,%eax
movl $10,%ebx
idivl %ebx
pushl %eax
pushl %edi
call L137
addl $8,%esp
popl %edx
popl %ecx
popl %eax
L177:
L176:
L175:
L173:
L169:
movl 12(%ebp),%eax
movl %eax,%esi
L170:
L167:
L165:
movl 12(%ebp),%eax
movl %eax,%eax
L166:
movl $0,%edx
movl %eax,%eax
movl $10,%ebx
idivl %ebx
movl %eax,%edi
L168:
movl $10,%ebx
movl %edi,%edi
imull %ebx,%edi
movl %esi,%ebx
subl %edi,%ebx
movl %ebx,%ebx
L174:
L172:
pushl %eax
pushl %ecx
pushl %edx
leal L171,%eax
pushl %eax
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
movl %ebx,%eax
addl %edi,%eax
pushl %eax
call chr
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
pushl %ebx
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L180
L244:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L186:
.4byte 1
.ascii "-"
L193:
.4byte 1
.ascii "0"
.globl L117
.type L117, @function
L117:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L209:
L205:
movl 12(%ebp),%ebx
movl %ebx,%edi
L206:
movl $0,%ebx
cmpl %ebx,%edi
jl L202
L203:
L208:
L198:
movl 12(%ebp),%ebx
movl %ebx,%edi
L199:
movl $0,%ebx
cmpl %ebx,%edi
jg L195
L196:
L201:
L194:
pushl %eax
pushl %ecx
pushl %edx
leal L193,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
L197:
movl %eax,%eax
L204:
movl %eax,%eax
jmp L245
L202:
L207:
L187:
pushl %eax
pushl %ecx
pushl %edx
leal L186,%ebx
pushl %ebx
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L191:
movl %ebp,%edi
L190:
L188:
L189:
pushl %eax
pushl %ecx
pushl %edx
movl $0,%ebx
movl 12(%ebp),%eax
movl %ebx,%ebx
subl %eax,%ebx
pushl %ebx
pushl %edi
call L137
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L204
L195:
L200:
movl %ebp,%ebx
L192:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%eax
pushl %eax
pushl %ebx
call L137
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L197
L245:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L210:
.4byte 1
.ascii "\n"
L216:
.4byte 1
.ascii " "
.globl L118
.type L118, @function
L118:
pushl %ebp
movl %esp,%ebp
subl $100,%esp
pushl %ebx
pushl %edi
pushl %esi
L229:
L225:
L228:
movl 8(%ebp),%ebx
movl %ebx,%esi
L215:
L212:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L213:
movl 12(%ebp),%ebx
movl %ebx,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
L214:
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
movl $4,%ebx
movl %edi,%edi
imull %ebx,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%ebx
addl %edi,%ebx
movl (%ebx),%ebx
pushl %ebx
pushl %esi
call L117
addl $8,%esp
popl %edx
popl %ecx
popl %eax
L223:
L217:
pushl %eax
pushl %ecx
pushl %edx
leal L216,%ebx
pushl %ebx
call print
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L222:
movl 8(%ebp),%ebx
movl %ebx,%edi
L221:
L218:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
pushl %ebx
call recordNilCheck
addl $4,%esp
popl %edx
popl %ecx
popl %eax
L219:
movl 12(%ebp),%ebx
movl %ebx,%esi
L220:
pushl %eax
pushl %ecx
pushl %edx
movl $1,%ebx
movl $4,%eax
movl %ebx,%ebx
imull %eax,%ebx
movl %esi,%eax
addl %ebx,%eax
movl (%eax),%eax
pushl %eax
pushl %edi
call L118
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
L226:
movl %eax,%eax
jmp L246
L224:
L227:
L211:
pushl %eax
pushl %ecx
pushl %edx
leal L210,%eax
pushl %eax
call print
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L226
L246:
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
subl $112,%esp
pushl %ebx
pushl %edi
pushl %esi
L239:
L0:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
L1:
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
L230:
movl $-108,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
L231:
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L116
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%ebx)
L235:
movl $-112,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%edi
L236:
L232:
movl $-104,%ebx
movl %ebx,%ebx
addl %ebp,%ebx
movl %ebx,%ebx
L233:
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %esi,(%ebx)
L234:
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L116
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,(%edi)
L238:
movl %ebp,%ebx
L237:
pushl %eax
pushl %ecx
pushl %edx
movl -112(%ebp),%eax
pushl %eax
pushl %ebx
call L118
addl $8,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,%eax
jmp L247
L247:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
