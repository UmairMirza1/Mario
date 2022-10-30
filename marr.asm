[org 0x100]

jmp start

win db 'You Won'
lose db 'You Lost'
Hurd1 dw 3240  ; Default location of Hurdle 1
Hurd2 dw 3440  ; Default location of Hurdle 2
Hurd3 dw 3324 ; Default location of Hurdle 3
oldkb: dd 0
MarioLocation dw 3680
enemyone: dw 3728
enemytwo: dw 3770
MonsterLocation: dw 20
MarioFaceR: dw 0 
MarioFaceL: dw 0 
MarioFaceM: dw 0 
Marioleg: dw 0


Mariohandr: dw 0
sMariohandr: dw 0
sMariohandr1: dw 0

sMariohandl: dw 0
sMariohandl1: dw 0
Mariohandl: dw 0
Mariohandl1: dw 0
Mariohandr1: dw 0
Torso1: dw 0
Torso2: dw 0
stone: dw 20
stone2: dw 38



MakeKingdom:
push ax
push es
push di

mov di,3518
mov ax,0xb800
mov es,ax 
mov ax , 0x08b2
mov [es:di], ax
add di,160
mov ax , 0x08b2
mov [es:di], ax
add di,160
mov ax , 0x08b2
mov [es:di], ax
add di,160
mov ax , 0x08b2
mov [es:di], ax

sub di,2
mov ax , 0x08b2
mov [es:di], ax
sub di,160
mov ax , 0x08b2
mov [es:di], ax
sub di,160
mov ax , 0x08b2
mov [es:di], ax
sub di,160
mov ax , 0x08b2
mov [es:di], ax





pop di 
pop es 
pop ax 
ret

Makestone:
push bp 
mov bp,sp
push ax
push es
push di

mov ax,0xb800
mov es, ax
mov di,[bp+4]
mov ax ,0x73CE
mov [es:di],ax 

pop di
pop es
pop ax
pop bp
ret 2

Removestone:
push bp 
mov bp,sp
push ax
push es
push di

mov ax,0xb800
mov es, ax
mov di,[bp+4]
mov ax ,0x00db
mov [es:di],ax 

pop di
pop es
pop ax
pop bp
ret 2

CallStone:

push word [stone]
call Removestone

add word [stone], 320
push word [stone]
call Makestone 



ret

CallStone2:

push word [stone2]
call Removestone

add word [stone2], 320
push word [stone2]
call Makestone 


ret


delay:           ;Subroutine for delaying procedures
push cx
push dx
mov cx,2        ;counts to ffffh 5 times and returns

lop1:

mov dx, 0xffff
lop2:
sub dx,1
cmp dx,0
jne lop2

sub cx,1
cmp cx,0
jne lop1
pop dx
pop cx
ret



clrscr:    ;Clearscreen subroutine
push ax
mov ah,00
mov al, 03h
int 10h
pop ax
ret



makemario:  ; Subroutine to make mario

push bp 
mov  bp,sp
push es   
push ax


mov ax,0xb800
mov es,ax 
mov ax,0x072D
mov di,[bp+4]

;add di,2
sub di,2
mov word [Mariohandl], di 
add di,2
;-------
add di,160
mov word [sMariohandl1], di                  ; ------- Storing for hand collisions with stone
sub di,160
;--------
mov [es:di],ax
add di,2 
;-------
add di,160
mov word [sMariohandl], di                  ; ------- Storing for hand collisions with stone
sub di,160
;--------
mov [es:di],ax   ; left hand


;middle box 
add di,2 
mov ax, 0x04Db
mov [es:di],ax
add di,2
mov word [Torso1], di
mov [es:di],ax
add di,2
mov word [Torso2], di
mov [es:di],ax
sub di,160
mov [es:di],ax
sub di,2 
mov [es:di] ,ax
sub di,2 
mov [es:di] ,ax
;neck
sub di, 158
mov ax, 0x07db
mov [es:di], ax 

;face 
sub di,162
mov word [MarioFaceL] , di 
mov ax,0x74FE
mov [es:di] , ax
add di,2
mov ax,0x00db
mov word [MarioFaceM], di
mov [es:di] , ax
add di,2
mov ax,0x74FE
mov word [MarioFaceR] , di 
mov [es:di] , ax 

;righthand
add di,480
add di,2
mov ax,0x072D
mov [es:di] , ax
mov word [Mariohandr1],di 
;-------
add di,160
mov word [sMariohandr1], di                  ; ------- Storing for hand collisions with stone
sub di,160
;--------

add di,2
mov word [Mariohandr], di
;-------
add di,160
mov word [sMariohandr], di                  ; ------- Storing for hand collisions with stone
sub di,160
;--------
mov [es:di] , ax
;rightleg

sub di, 6
add di,158  
mov ax, 0x077c
mov [es:di],ax
add di,4
mov word [Marioleg] , di
mov [es:di],ax
 

pop ax
pop es
mov sp , bp
pop bp
ret 2
;;


Hurdle:

push bp 
mov  bp,sp
push es 
push ax
push bx
push di


mov ax, 0xb800
mov es, ax 

mov di, [bp+4]
mov dx,2

; Top
Topbox1:
mov cx,7

printline1:
mov si,di
mov ax , 0x02db         ; re coloured
mov [es:di],ax          ; Bottombox of dimension 2 by 7
add di,2
sub cx,1 
cmp cx,0
jnz printline1
mov di,si
add di,148
sub dx,1
cmp dx,0
jnz Topbox1

mov ax,0x08db
add di, 6
mov [es:di],ax 
add di ,160
mov [es:di],ax
add di ,160
mov [es:di],ax



pop di
pop bx
pop es
pop ax
pop bp

ret 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LeftBound:    ;Boundary check for left end
 
mov word [MarioLocation], 3680
push word [MarioLocation]
call makemario
jmp exit

RightBound:   ; Boundary check for right end

mov word [MarioLocation], 3826
push word [MarioLocation]
call makemario
jmp exit

;;;;;;;;;;;;;;;;;;;;
RightJumpBound:   ; check for jump bound so it doesnt go out of screen

mov word [MarioLocation],3186
push 3186
call makemario
call delay

call clrscr

 
push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom

mov word [MarioLocation],3826
push 3826
call makemario
jmp exit


mariojump:    ; Jump implementation


call clrscr

 
push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom
sub word [MarioLocation],640  ; moves mario up 
push word [MarioLocation]

call makemario
;call delay
;------------------------------------------------------
call clrscr

push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom
add word [MarioLocation],24   ; moves right over the hurdle
cmp word [MarioLocation],3186
jg RightJumpBound
push word [MarioLocation]

call makemario
call delay
;------------------------------------------
call clrscr


push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom
add word [MarioLocation],640
push word [MarioLocation]

call makemario
call delay

jmp exit

;-------------------------------------------------------------------------------

kbisr: 

	push ax
	push di
	push cx
	push dx

in al,0x60
cmp al, 48h ; up
je mariojump

;cmp al, 50h down 

cmp al,4bh  ; left
je marleft

cmp al, 4dh  ; right
je movemarioright

jmp nomatch


nomatch:
	pop dx
	pop cx
	pop di
	pop ax
	jmp far [cs:oldkb] ; call original ISR

exit:
	mov al, 0x20
	out 0x20, al ; send EOI to PIC

	pop dx
	pop cx
	pop di
	pop ax

iret ; return from interrupt
;-------------------------------------------------------------------------------
movemarioright:    ; ----- RIGHT MOVEMENT
cmp word [MarioFaceR],3238 
je exit

cmp word [MarioFaceR],3278 
je exit
cmp word [MarioFaceR],3322
je exit

call clrscr

push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom
add word [MarioLocation],2
cmp word[MarioLocation],3826   ; checks if its gonna get out of bound
jg RightBound
push word [MarioLocation]
call makemario 
jmp exit

;;;;;--------------------------------------------------
marleft:
cmp word [MarioFaceL],3254
je exit

cmp word [MarioFaceL],3294 
je exit
cmp word [MarioFaceL],3338
je exit
call clrscr           ;----- LEFT MOVEMENT 

push 3400
call Hurdle

push word [Hurd2]
call Hurdle ; Hurdle 2

push word [Hurd3]
call Hurdle ; Hurdle 3
call MakeKingdom
sub word [MarioLocation],2

cmp  word[MarioLocation],3680  ; checks if its gonna get out of bound
jl LeftBound
push word [MarioLocation]
call makemario 
jmp exit


;;;;;;;;;;;;;;;;;;

Enemy:

push bp
mov bp,sp
push ax
push di


mov di,[bp+4]
mov ax,0xb800
mov es, ax
mov ax,0x585E
mov [es:di],ax
add di,160
mov ax,0x07cc
mov [es:di],ax
add di,2 
mov ax,0x07cc
mov [es:di],ax
sub di,160
mov ax,0x585E
mov [es:di],ax

pop di
pop ax
pop bp
ret 2
;;;;;;;;

RemoveEnemy:

push bp
mov bp,sp
push ax
push di


mov di,[bp+4]
mov ax,0xb800
mov es, ax
mov ax,0x00db
mov [es:di],ax
add di,160
mov [es:di],ax
add di,2 
mov [es:di],ax
sub di,160
mov [es:di],ax

pop di
pop ax
pop bp
ret 2

enemyoneright:
push word [enemyone]
call RemoveEnemy
add word [enemyone],2
push word [enemyone]
call Enemy
ret
enemyoneleft
push word [enemyone]
call RemoveEnemy
sub word [enemyone],2
push word [enemyone]
call Enemy
ret


enemytworight:
push word [enemytwo]
call RemoveEnemy
add word [enemytwo],2
push word [enemytwo]
call Enemy
ret

enemytwoleft:
push word [enemytwo]
call RemoveEnemy
sub word [enemytwo],2
push word [enemytwo]
call Enemy
ret
;------------------------------
Removemonster:
push bp
mov bp,sp
push ax
push di
push es

mov di,[bp+4]
mov ax,0xb800
mov es, ax
mov ax,0x00db
mov [es:di],ax
add di,2
mov [es:di],ax
add di,2 
mov [es:di],ax
add di,160
mov [es:di],ax
sub di,2
mov [es:di],ax
sub di,2
mov [es:di],ax

pop es
pop di
pop ax
pop bp
ret 2  


Monster:

push bp
mov bp,sp
push ax
push di
push es

mov di,[bp+4]
mov ax,0xb800
mov es, ax
mov ax,0x53db
mov [es:di],ax
add di,2
mov [es:di],ax
add di,2 
mov [es:di],ax
add di,160
mov ax,0x532F
mov [es:di],ax
sub di,2
mov ax,0x88df
mov [es:di],ax
sub di,2
mov ax,0x935c
mov [es:di],ax

pop es
pop di
pop ax
pop bp
ret 2  

monsterright:
push word [MonsterLocation]
call Removemonster
add word [MonsterLocation],4
push word [MonsterLocation]
call Monster

ret
monsterleft:
push word [MonsterLocation]
call Removemonster
sub word [MonsterLocation],4
push word [MonsterLocation]
call Monster
ret




Checkcollision:
push ax
push bx
push cx

mov ax,[stone]
cmp word [MarioFaceR],ax
je ttl22
cmp word [MarioFaceL],ax
je ttl22
cmp word [MarioFaceM],ax
je ttl22
cmp word [sMariohandr1],ax   ;---COLLISIONS WITH FIRST STONE
je ttl22
cmp word [sMariohandr],ax
je ttl22
cmp word [sMariohandl1],ax
je ttl22

jmp nextt
ttl22:
jmp tl22

nextt:
cmp word [sMariohandl],ax
je  tl22
mov ax,[stone2]
cmp word [MarioFaceR],ax
je tl22
cmp word [MarioFaceL],ax 
je tl22
cmp word [MarioFaceM],ax    ;---- COLLISIONS WITH SECOND STONE
je tl22
cmp word [sMariohandr1],ax
je tl22
cmp word [sMariohandr],ax
je tl22
cmp word [sMariohandl1],ax
je tl22
cmp word [sMariohandl],ax
je  tl22


mov ax, [enemyone]
mov bx,ax
add bx,2
cmp word [Torso1],bx ;-----------------ENEMY CHECKS 
je  tl22
cmp word [Torso2],bx
je  tl22
cmp word [Mariohandr1],ax
je tl22
cmp word [Mariohandl1],ax
je tl22
cmp word [Mariohandr],ax
je tl22
cmp word [Mariohandl],ax
je tl22
mov ax ,[enemytwo]
cmp word [Mariohandr],ax
je tl22
cmp word [Mariohandl],ax
je tl22
jmp nextcond
;--
tl22:
jmp l22
;
nextcond:
cmp word [Torso2],ax
je  tl22
cmp word [Torso1],ax
je  tl22
mov cx,3838
cmp word [Mariohandr],cx     ;--------- WINNING CONDITION
je  takel33
pop cx
pop bx
pop ax
ret

takel33:
jmp l33


l22:
jmp Winn

l33
jmp Lost


Winn:

call clrscr

mov ah,13h
mov al,0
mov bh,0
mov bl,9
mov cx,8
mov dh,8
mov dl,40
push ds 
pop es
mov bp,lose
int 10h
cli
jmp Winn

Lost:
call clrscr
mov ah,13h
mov al,0
mov bh,0
mov bl,9
mov cx,7
mov dh,8
mov dl,40
push ds 
pop es
mov bp,win
int 10h
cli
jmp Lost


start:

call clrscr

 
push 3400
call Hurdle
push word [Hurd2]
call Hurdle ; Hurdle 2 
push word [Hurd3]
call Hurdle ; Hurdle 3
push word [MarioLocation]
call makemario 

call MakeKingdom

	xor ax, ax
	mov es, ax 
	mov ax, [es:9*4]                 ;STORING OLD OR ORGINAL ISR
	mov [oldkb], ax 
	mov ax, [es:9*4+2]
	mov [oldkb+2], ax 
	

cli 
	mov word [es:9*4], kbisr         ;HOOKING KEYBOARD ISR
	mov [es:9*4+2], cs 
sti 
jmp l11


l11:

moveright:  
mov word [stone2] ,54
call delay
call enemyoneright
call enemytworight
call monsterright
call CallStone
call Checkcollision

cmp word [enemyone],3762
je moveleft
jne moveright

moveleft:


mov word [stone], 20

;push word [stone]
;call Removestone
call delay

call enemyoneleft
call enemytwoleft
call monsterleft
call CallStone2
call Checkcollision
cmp word [enemyone], 3728
je moveright
jne moveleft


jmp  l11






exitt:
cli
mov ax,0x4c00
int 21h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
