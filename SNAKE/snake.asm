[org 0x0100]
jmp start
;/////////////////////////////////////////////////////////////////////////////
clrsc:             ;/////////clearscreen function////////
push ax
push di
push cx
mov ax,0xb800      ;   I/O segment start
mov es,ax
mov di,0           ;   index iterator
mov ax,0x4420      ;   attribute(higher)+ascii(lower)
mov cx,2000
cld
rep stosw
pop cx
pop di
pop ax
ret

;//////////////////////////level 1/////////////////////////////
line:
;//print a line
push cx
mov cx,80
rep stosw
inc si
pop cx
jmp l1

boundary:        ;////print boundary function
pusha
mov ax,0xb800 ;video memory address
mov es,ax
mov si,0       ;counter for total lines 25
mov di,0       ;index of video screen
mov ax ,0x1120 ;attribute+char
cld
l1:
cmp si,25
je end1
cmp si,2    ;if first line
je line
cmp si,24   ;if last line
je line
stosw   ;at start of line
add di,156
stosw   ;at end of line
inc si
jmp l1
end1:
popa
ret
;//////////////////////////level 2/////////////////////////////
line1:
;//print a line
push cx
mov cx,80
rep stosw
inc si
pop cx
jmp l11

line2:
;//print a line
push cx
add di,40;go to midddle of line
mov cx,40
rep stosw
inc si
sub di,122;go back to same position
pop cx
jmp l11
boundary1:        ;////print boundary function
pusha
mov ax,0xb800 ;video memory address
mov es,ax
mov si,0       ;counter for total lines 25
mov di,0       ;index of video screen
mov ax ,0x1120 ;attribute+char
cld
l11:
cmp si,27
je end11
cmp si,2    ;if first line
je line1
cmp si,26   ;if last line
je line1
stosw   ;at start of line
cmp si,8
je line2
cmp si,16 
je line2
add di,156
stosw   ;at end of line
inc si
jmp l11
end11:
popa
ret
;/////////////////////////////////////////////////////////////////////////////
myEnd2:
push 5    ;timeOfBeep
push 1500     ;frequency
call beep
dec word[life]
cmp word[life], 0
jne endksub1
mov di,12;for life
push word[life]
call printnum
call endMessage
jmp exitProgram

endksub1:
mov byte[bool1],0
popa
ret
;jmp endk

exte4:
push 3    ;timeOfBeep
push 1000     ;frequency
call beep
add word[points], 2
add word[extend], 4
l1000u4:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1000u4
extee4:
push 1    ;timeOfBeep
push 400     ;frequency
call beep
inc word[snakeSize]	;size of snake will be increased
mov di, [headPos]
push di
mov word[es:di], 0x7740
sub word[headPos], 160
mov bx, word[headPos] ;head position saved
;creating new space
mov si, word[head] ;getting to location of new space
shl si, 1
;now saving value at new position
pop di
mov word[pos+si], di
;now updating the rest of the values
inc word[head]
mov di, [headPos]
mov word[es:di], 0x004f
dec word[extend]
popa
jmp endk


up:
pusha
push cs
pop ds
mov ax, 0xb800
mov es, ax ; point es to video base

mov si,[headPos]
sub si, 160
mov ax,word[cs:fruit]
cmp word[es:si], ax
je exte4
cmp word[extend], 0
jne extee4
push 1    ;timeOfBeep
push 400     ;frequency
call beep
mov di,[headPos]
sub di, 160
cmp word[es:di], 0x1120;boundary
je myEnd2
cmp word[es:di], 0x7740
je myEnd2
mov si, 0
mov di, [pos+si]
mov word[es:di],0x4420 ;remove previous
mov cx, word[head]
dec cx
l7:
mov di, [pos+si+2]
mov [pos+si], di
add si, 2
loop l7
mov di, [headPos]
mov [pos+si], di
sub word[headPos], 160
mov di, [headPos]
mov ax,word[cs:fruit]
cmp word[es:di], ax;fruit
jne skip
l1003:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1003
skip:
mov si, 0
mov cx, [head]
loop6:
mov di, [pos+si]
mov word[es:di], 0x7740
add si, 2
add di, 2
loop loop6
mov di, [headPos]
mov word[es:di], 0x004f
popa
jmp endk



exte3:
push 3    ;timeOfBeep
push 1000     ;frequency
call beep
add word[points], 2
add word[extend], 4
l1000u3:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1000u3
extee3:
push 1    ;timeOfBeep
push 400     ;frequency
call beep
inc word[snakeSize]	;size of snake will be increased

mov di, [headPos]
push di
mov word[es:di], 0x7740

add word[headPos], 160
mov bx, word[headPos] ;head position saved
;creating new space
mov si, word[head] ;getting to location of new space
shl si, 1

;now saving value at new position
pop di
mov word[pos+si], di
;now updating the rest of the values

inc word[head]
mov di, [headPos]
mov word[es:di], 0x004f
dec word[extend]
popa
jmp endk

down:
pusha
push cs
pop ds
mov ax, 0xb800
mov es, ax ; point es to video base

mov si,[headPos]
add si, 160
mov ax,word[cs:fruit]
cmp word[es:si], ax
je exte3
cmp word[extend], 0
jg extee3
push 1    ;timeOfBeep
push 400     ;frequency
call beep
mov di,[headPos]
add di, 160
cmp word[es:di], 0x1120;boundary
je myEnd2
cmp word[es:di], 0x7740
je myEnd2

mov si, 0
mov di, [pos+si]
mov word[es:di],0x4420 ;remove previous
mov si, 0
mov cx, word[head]
dec cx
l6:
mov di, [pos+si+2]
mov [pos+si], di
add si, 2
loop l6
mov di, [headPos]
mov [pos+si], di
add word[headPos], 160
mov ax,word[cs:fruit]
cmp word[es:di], ax
jne skip2
l1002:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1002

skip2:
mov di, [headPos]
mov si, 0
mov cx, [head]
loop3:
mov di, [pos+si]
mov word[es:di], 0x7740
add si, 2
add di, 2
loop loop3
mov di, [headPos]
mov word[es:di], 0x004f
popa
jmp endk
;/////////////////////////////////////////////////
myEnd:
push 5    ;timeOfBeep
push 1500     ;frequency
call beep
dec word[life]
cmp word[life], 0
jne endksub
mov di,12;for life
push word[life]
call printnum
call endMessage;
jmp exitProgram

endksub:
mov byte[bool1],0
popa
jmp endk

exte2:
push 3    ;timeOfBeep
push 1000     ;frequency
call beep
add word[points], 2
add word[extend], 4
l1000u2:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1000u2
extee2:
push 1    ;timeOfBeep
push 400     ;frequency
call beep
inc word[snakeSize]	;size of snake will be increased

mov di, [headPos]
push di
mov word[es:di], 0x7740

sub word[headPos], 2
mov bx, word[headPos] ;head position saved
;creating new space
mov si, word[head] ;getting to location of new space
shl si, 1

;now saving value at new position
pop di
mov word[pos+si], di
;now updating the rest of the values

inc word[head]
mov di, [headPos]
mov word[es:di], 0x004f
dec word[extend]
popa
jmp endk


left:
pusha
push cs
pop ds
mov ax, 0xb800
mov es, ax ; point es to video base

mov si,[headPos]
sub si, 2
mov ax,word[cs:fruit]
cmp word[es:si], ax
je exte2
cmp word[extend], 0
jg extee2
push 1    ;timeOfBeep
push 400     ;frequency
call beep
mov di,[headPos]
sub di, 2
cmp word[es:di], 0x1120;boundary
je myEnd2
cmp word[es:di], 0x7740
je myEnd2

mov si, 0
mov di, [pos+si]
mov word[es:di],0x4420 ;remove previous
mov si, 0
mov cx, word[head]
dec cx
l8:
mov di, [pos+si+2]
mov [pos+si], di
add si, 2
loop l8
mov di, [headPos]
mov [pos+si], di
sub word[headPos], 2
mov di, [headPos]
mov ax,word[cs:fruit]
cmp word[es:di], ax
jne skip3
l1001:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1001

skip3:
mov si, 0
mov cx, [head]
loop7:
mov di, [pos+si]
mov word[es:di], 0x7740
add si, 2
add di, 2
loop loop7
mov di, [headPos]
mov word[es:di], 0x004f
popa
jmp endk

exte:
push 3    ;timeOfBeep
push 1000     ;frequency
call beep
add word[points], 2
add word[extend], 4
l1000u:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1000u
extee:
push 1    ;timeOfBeep
push 400     ;frequency
call beep
inc word[snakeSize]	;size of snake will be increased

mov di, [headPos]
push di
mov word[es:di], 0x7740

add word[headPos], 2
mov bx, word[headPos] ;head position saved
;creating new space
mov si, word[head] ;getting to location of new space
shl si, 1

;now saving value at new position
pop di
mov word[pos+si], di
;now updating the rest of the values

inc word[head]

mov di, [headPos]
mov word[es:di], 0x004f


dec word[extend]
popa
jmp endk


right: 
pusha
push cs
pop ds
mov ax, 0xb800
mov es, ax ; point es to video base

mov si, [headPos]
add si, 2
mov ax,word[cs:fruit]
cmp word[es:si], ax
je exte
cmp word[extend], 0
jg extee
push 1    ;timeOfBeep
push 400     ;frequency
call beep
mov di,[headPos]
add di, 2
cmp word[es:di], 0x1120;boundary
je myEnd
cmp word[es:di], 0x7740
je myEnd

mov si, 0
mov di, [pos+si]
mov word[es:di],0x4420 ;remove previous
mov si, 0
mov cx, word[head]
dec cx
l5:
mov di, [pos+si+2]
mov [pos+si], di
add si, 2
loop l5
mov di, [headPos]
mov [pos+si], di
add word[headPos], 2
mov di, [headPos]
mov ax,word[cs:fruit]
cmp word[es:di],ax
jne skip4
l1000:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1000
;/////////

skip4:
mov si, 0
mov cx, [head]
loop2:
mov di, [pos+si]
mov word[es:di], 0x7740
add si, 2
add di, 2
loop loop2
mov di, [headPos]
mov word[es:di], 0x004f
 popa
jmp endk

printsnake:
cmp byte[cs:bool1],0x11        ;'w'
je up
cmp byte[cs:bool1],0x1f        ;'s'
je down
cmp byte[cs:bool1],0x1e        ;'a'
je left
cmp byte[cs:bool1],0x20        ;'d' 510907
je right
endk:
ret
kbisr:
push ax
in al,0x60         ;keyboard port address
cmp al,0x11        ;'w'
je kend1
cmp al,0x1f        ;'s'
je kend2
cmp al,0x1e        ;'a'
je kend3
cmp al,0x20        ;'d' 510907
je kend4
kendl:
mov al,0x20
out 0x20,al
pop ax
iret
;-------opposite directions checks
kend1:
cmp byte[cs:bool1], 0x1f
je kendl
mov [cs:bool1],al
jmp kendl
kend2:
cmp byte[cs:bool1], 0x11
je kendl
mov [cs:bool1],al
jmp kendl
kend3:
cmp byte[cs:bool1], 0x20
je kendl
mov [cs:bool1],al
jmp kendl
kend4:
cmp byte[cs:bool1], 0x1e
je kendl
mov [cs:bool1],al
jmp kendl

;randomnumbergenerator
loc:
push bp
mov bp,sp
push ax
push cx
mov ax,[bp+6]
mov cl,80
mul cl
add ax,[bp+4]
shl ax,1
mov [bp+8],ax

pop ax
pop cx
pop bp
ret 4
rng:
pusha
mov al,00
out 0x70,al
jmp d1
d1:
in al,0x71
mov ah,0
mov bx,ax
mov cl,80
div cl
mov al,ah
xor ah,ah
mov si,ax
mov ax,bx
mov cl,25
div cl
mov al,ah
xor ah,ah
mov di,ax
push 0
push di
push si
call loc
pop di
mov ax,0xb800
mov es,ax
; check
cmp di,320
jl lab
cmp word[es:di],0x1120;boundary
je lab
cmp word[es:di],0x7740;body
je lab
cmp word[es:di],0x004f;head
je lab
add word[cs:fruit],257 ;changing fruit
mov ax,word[cs:fruit]
mov word[es:di],ax
noprint:
popa
ret 
lab:
mov word[cs:randloc],1
jmp noprint
; timer interrupt service routine

printnum: 
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
;push di
mov ax, 0xb800
mov es, ax ; point es to video base

mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
;
cmp ax,10
jge nextdigit
mov word[es:di+2],0x4420;space
;

nextdigit: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again

nextpos: 
pop dx ; remove a digit from the stack
mov dh, 0x40 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
;pop dihi

pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2

; timer interrupt service routine
pleaseEnd;
mov di,12;for life
push word[life]
call printnum
call endMessage
jmp exitProgram

myEnd1:
push 5    ;timeOfBeep
push 1500     ;frequency
call beep
dec word[life]
cmp word[life], 0
je pleaseEnd
mov word[cs:bool1],0
mov word[cs:min],1
jmp ll

timer:
pusha
push cs
pop ds
add word [cs:tickspeed],55 ; increment tickspeed
mov ax,[cs:speed]
cmp word [cs:tickspeed],ax ; speed controller
jl lop
mov word[cs:tickspeed],0
call printsnake
cmp word[cs:countsec],20;speed double after 20 seconds
jl lop
mov word[cs:countsec],0
mov ax, word[cs:speed]
shr ax,1
mov word[cs:speed],ax

lop:
;cmp word[cs:points],8 ;updateLevel
;je updateLevel;
;call boundary
mov di,12;for life
push word[life]
call printnum
mov di,96;for points
push word[points]
call printnum

mov di, 154 ; point di to 70th column
s1:
add word [cs:tickcount],55; increment tick count
cmp word [cs:tickcount],1000
jl second
mov word [cs:tickcount],0
cmp word[cs:sec],0
jg nai
;--------timeout check
cmp word[cs:min],0     ;reset check
jg ll
cmp word[cs:head],240  ;end check
jl myEnd1
add word[cs:points],10 ;bonus
call endMessage1
ll:
mov word[cs:sec],60
dec word[cs:min]
nai:
dec word[cs:sec]
inc word[cs:countsec] ;for speed controller
second:
push word [cs:sec]
call printnum ; print tick count
mov di, 148 ; move to next screen location

push word [cs:min]
call printnum ; print tick count
sub di,12 ; move to next screen location
mov al, 0x20
out 0x20, al ; end of interrupt
popa
iret ; return from interrupt

beep:
;The following code fragment generates a beep with a frequency of 261.63 Hz (middle C on a piano keyboard) and a duration of approximately one second:
push bp
mov bp,sp
push ax 
push bx
push cx
mov     al, 182         ; Prepare the speaker for the
out     43h, al         ;  note.
mov     ax, [bp+4]        ; Frequency number (in decimal)
                        ;  for middle C.
out     42h, al         ; Output low byte.
mov     al, ah          ; Output high byte.
out     42h, al 
in      al, 61h         ; Turn on note (get value from
                        ;  port 61h).
or      al, 00000011b   ; Set bits 1 and 0.
out     61h, al         ; Send new value.
mov     bx, [bp+6]          ; Pause for duration of note.
.pause1:
mov     cx, 65535
.pause2:
dec     cx
jne     .pause2
dec     bx
jne     .pause1
in      al, 61h         ; Turn off note (get value from port 61h).
and     al, 11111100b   ; Reset bits 1 and 0.
out     61h, al         ; Send new value.
pop cx
pop bx
pop ax
pop bp
ret 4
;///////////////////////////////////level 1
begin:
call clrsc
call boundary
push cs
pop ds
l1004:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l1004
mov di,80        ;index of video screen
mov si,0         ;index of points array
mov ah,0x40
loopss:
mov al,byte[displayPoints+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayPoints+si],0
jne loopss

mov di,2       ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loops:
mov al,byte[displayLife+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayLife+si],0
jne loops


mov di,482 
mov si,0         ;index of snake array
mov ah,0x77
mov cx, word[snakeSize]
dec cx
loop1:

mov al,byte[sap+si]
mov word[es:di],ax
inc si
add di,2
loop loop1
mov ax, 0x004f
mov al,byte[sap+si]
mov word[es:di],ax

end:
jmp loopst

;////////////////////////////////
begin1:
call clrsc
call boundary1
push cs
pop ds
l10041:
mov word[cs:randloc],0
call rng
cmp word[cs:randloc],1
je l10041
mov di,80        ;index of video screen
mov si,0         ;index of points array
mov ah,0x40
loopss1:
mov al,byte[displayPoints+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayPoints+si],0
jne loopss1

mov di,2       ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loops1:
mov al,byte[displayLife+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayLife+si],0
jne loops1


mov di,482 
mov si,0         ;index of snake array
mov ah,0x77
mov cx, word[snakeSize]
dec cx
loop11:

mov al,byte[sap+si]
mov word[es:di],ax
inc si
add di,2
loop loop11
mov ax, 0x004f
mov al,byte[sap+si]
mov word[es:di],ax
;end11:
jmp loopst


startMessage:
pusha
mov ax,0xb800

mov di,1960      ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loopssss:
mov al,byte[displayStart+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayStart+si],0
jne loopssss

mov di,2120      ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loopsssss1:
mov al,byte[displayStart1+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayStart1+si],0
jne loopsssss1

mov di,2280      ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loopssss2:
mov al,byte[displayStart2+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayStart2+si],0
jne loopssss2

popa
ret

endMessage:
pusha
mov ax,0xb800

mov di,482       ;index of video screen
mov si,0         ;index of life array
mov ah,0x40
loopsss:
mov al,byte[displayEnd+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayEnd+si],0
jne loopsss
mov si,0
cmp di,3996
jl loopsss 
popa
ret

endMessage1:
pusha
mov word[cs:bool1], 0
call clrsc
mov ax,0xb800
mov di,0       ;index of video screen
mov si,0         ;index of congratulate array
mov ah,0x40
loopsssss:
mov al,byte[displayEnd1+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayEnd1+si],0
jne loopsssss
mov si,0
cmp di,3996
jl  loopsssss 

mov di,1972       ;index of video screen
mov si,0         ;index of Your score is array
mov ah,0x05
loopssss1:
mov al,byte[displayEnd2+si]
mov word[es:di],ax
inc si
add di,2
cmp byte[displayEnd2+si],0
jne loopssss1
push word[cs:points]
call printnum

popa
ret
;/////////////////////////////////////////////////////////////////////////////
prebegin:
call clrsc
call startMessage
loopa:
mov ah,0
int 0x16
cmp al,0x61 ;a
je begin
cmp al,0x62 ;b
je begin1
jmp loopa






;/////////////////////////////////////////////////////////////////////////////

start: 
jmp prebegin
loopst:
xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr+2], ax ; save segment of old routine
cli ; disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2
mov word [es:8*4], timer ; store offset at n*4
mov [es:8*4+2], cs ; store segment at n*4+2
sti ; enable interrupts


exitProgram:
mov dx, start ; end of resident portion
add dx, 15    ; round up to next para
mov cl, 4
shr dx, cl    ; number of paras
mov ax, 0x3100 ; terminate and stay resident
int 0x21
iterator:dw 0
sap: db  '@@@@@@@@@@@@@@@@@@@O';0x076f ;attribute+char(O)
snakeSize: dw 20
displayLife:dw 'LIFE:',0
life:dw 3
pos: dw 482,484,486,488,490,492,494,496,498,500,502,504,506,508,510,512,514,516,518,520
headPos: dw 520
head:dw 20
extend:dw 0
bool1:db 0   ;direction
tickcount: dw 0
randtick:dw 0
xpos:dw 0
ypos:db 0
randloc :dw 0
min:dw 4
sec:dw 0
paa: dw 0
direction: db 0
oldisr: dd 0
speed:dw 1000
tickspeed:dw 0
countsec :dw 0
points:dw 0 ;points
displayPoints:db 'SCORE:',0
displayEnd:db ' GAME-OVER ',0
displayEnd1:db '   CONGRATULATIONS ',0
displayEnd2:db '    YOUR SCORE IS:',0
displayStart:db ' WELCOME TO SNAKE WORLD ',0
displayStart1:db ' PRESS "a" for FIRST STAGE ',0
displayStart2:db ' PRESS "b" for SECOND STAGE ',0
fruit:dw 0x1180
; subroutine to print a number at top left of screen
; takes the number to be printed as its parameter
