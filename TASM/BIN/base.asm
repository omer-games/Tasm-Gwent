.MODEL SMALL
.STACK 100h
.DATA
    hand DB 20 DUP(0) 
    seed DW 1234h     
    newline DB 0Dh,0Ah, '$' 
    space DB ' ', '$' 
    separator DB ', $'
    bar_separator DB ' | $'
    playerTurn DB 1 ; 1 means player's turn, need to make it random for who plays first

    computerWeather DB 10 DUP(0)
    cwl db 0 ; computer Weather Length
    computerHorn DB 10 DUP(0)
    chl db 0 ; computer horn Length
    csl db 0 ; computer sunshine cards amount
    
    ComputerWins DB 0
    PlayerWins DB 0

    ComputerGiveUp DB 0
    PlayerGiveUp DB 0

    board db 6*12 DUP(0) ; board, contains rows, each row contains cards 
    lens db 6 DUP(0) ; amount of card that are currently in the row
    computerMoves db 0
                ; Values:
                ; // 1-10 the value itself
                ; // 11 - spy 1
                ; // 12 - spy 5
                ; // 13 - kill last, 5
                ; // 14 - legendary, 10
                ; // 15 - legendary 15
                ; // 16 - horn
                ; // 17 - sword hurt
                ; // 18 - bow hurt
                ; // 19 - long hurt
                ; // 20 - sun shine
                
                ; Types:
                ; // 1 - swordsman
                ; // 2 - Archer
                ; // 3 - Longranger

    ; yeah yeah I know its aint needed but what u gonna do bout that?
    placeHolder db 0 
    placeHolder2 dw 0 

    sleepTime db 3


    bmpFileHandle dw ?  ; Store BMP file handle after opening it
    filename db ?,0
    filehandle dw ?
    Header db 54 dup (0)
    Palette db 256*4 dup (0)
    ScrLine db 320 dup (0)
    ErrorMsg db 'Error', 13, 10,'$'
    ;image size
    imageLength dw 320
    imageHeight dw 200
    ;image location
    ppx dw 10 ; x pos
    ppy dw 10 ; y pos
    ;location of pixel to Copy. used in copymatrix
    xValue dw 0
    yValue dw 0
    
    ; Place holders
    ph3 dw ?
    ph4 dw ?
    ph5 dw ?
    ph6 dw ?
    ph7 db ?

    isHand dw ?
    cardPos dw ?
    cardRow dw ?
    setName dw 0
    cardDead dw ?
    cardType db ?
    cardValue db ?
.CODE
    MAIN PROC
    MOV AX, @data
    MOV DS, AX  

    mov ax, 13h
    int 10h


    MenuScreen:

        MOV [byte ptr offset filename], 'M'
        MOV [byte ptr offset filename+1], 'e'
        MOV [byte ptr offset filename+2], 'n'
        MOV [byte ptr offset filename+3], '1'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 200
        MOV [imageLength], 320
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 0
        CALL PrintImage

        mov [ph6], 1

        pressWaitMenu:

            cmp [ph6], 1
            jne SK1
                MOV [byte ptr offset filename+3], '1'
            SK1:

            cmp [ph6], 2
            jne SK2
                MOV [byte ptr offset filename+3], '2'
            SK2:

            cmp [ph6], 3
            jne SK3
                MOV [byte ptr offset filename+3], '3'
            SK3:

            MOV [byte ptr offset filename], 'M'
            MOV [byte ptr offset filename+1], 'e'
            MOV [byte ptr offset filename+2], 'n'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 200
            MOV [imageLength], 320
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 0
            MOV [ppy], 0
            CALL PrintImage

            mov ah, 00h
            int 16h

            cmp al, 0Dh
            je EnterPressed
            
            cmp al, 'w'
            je MoveSelectedDown
            
            cmp al, 00h
            jne KeepGoingM1
            cmp ah, 48h
            je MoveSelectedDown 
            
            jmp KeepGoingM1
            
            
            MoveSelectedDown:
                cmp [ph6], 1
                jne KeepGoingM2
                    mov [ph6], 4 ; 3+1
                KeepGoingM2:
                dec [ph6]
                jmp pressWaitMenu
            KeepGoingM1:


            cmp al, 's'
            je MoveSelectedUp

            cmp al, 00h
            jne KeepGoingM3
            cmp ah, 50h
            je MoveSelectedUp

            jmp KeepGoingM3
            
            
            MoveSelectedUp:
                cmp [ph6], 3
                jne KeepGoingM4
                    mov [ph6], 0 ; 1-1
                KeepGoingM4:
                inc [ph6]
                jmp pressWaitMenu
            KeepGoingM3:



            jmp pressWaitMenu

    
    EnterPressed:

        cmp [ph6], 1
        jne KeepGoingEP1
            jmp MenuStart
        KeepGoingEP1:

        cmp [ph6], 2
        jne KeepGoingEP2
            jmp MenuRules
        KeepGoingEP2:

        MOV AX, 4C00h      
        INT 21h

    MenuRules:
    mov [ph7], '0'

    RulesLoop:
        inc [ph7]
        cmp [ph7], '6'
        jae RulesEnd
        MOV [byte ptr offset filename], 'R'
        MOV [byte ptr offset filename+1], 'u'
        MOV [byte ptr offset filename+2], 'l'
        mov al, [ph7]
        MOV [byte ptr offset filename+3], al
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 200
        MOV [imageLength], 320
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 0
        CALL PrintImage
        
        mov ah, 00h
        int 16h
        
        jmp RulesLoop

        RulesEnd:
        jmp MenuScreen

    MenuStart:

    mov playerTurn, 1

    push di
    xor di, di
    LoopStartClear_WNH:
        mov [computerWeather + di], 0
        mov [computerHorn + di], 0
        inc di
        cmp di, 10
        jl LoopStartClear_WNH
    pop di

    mov cwl, 0
    mov chl, 0
    mov csl, 0
    
    mov ComputerWins, 0
    mov PlayerWins, 0
    mov ComputerGiveUp, 0
    mov PlayerGiveUp, 0
    mov computerMoves, 0
    mov setName, 0


    CALL seed_random
    ; mov [seed], 1232 ; for consistent values to make debug easier :D
    ; mov [chl], 1
    ; mov [computerHorn], 2


    CALL fill_array     
    CALL computerHand
    ; mov [hand+1], 20
    ; mov [hand+3], 19
    ; mov [hand+5], 20
    ; mov [hand+7], 17
    call start_game

    ;mov [hand], 2
    ;mov [hand+1], 11
    ;mov [hand+2], 2
    ;mov [hand+3], 12
    ;mov [hand+4], 2
    ;mov [hand+5], 13
    ;mov [hand+6], 0
    ;mov [hand+7], 0
    ;mov [hand+8], 0
    ;mov [hand+9], 0



    ; CALL print_hand  
    ; mov dx, offset newline
    ; mov ah, 09h
    ; int 21h
    ; CALL print_board 
    ; mov dx, offset newline
    ; mov ah, 09h
    ; int 21h
    ; int 21h
    ; int 21h

    ; mov al, 2
    ; mov ah, 15
    ; mov bl, 0
    ; CALL place_card
    ; mov al, 2
    ; mov ah, 16
    ; mov bl, 0
    ; CALL place_card
    ; mov al, 2
    ; mov ah, 6
    ; mov bl, 0
    ; CALL place_card




    ; mov al, [hand+4]
    ; mov ah, [hand+5]
    ; mov bl, 0
    ; CALL place_card


    ; mov dx, offset newline
    ; mov ah, 09h
    ; int 21h
    ; CALL print_board 
    
    ; mov al, 2
    ; call calc_row
    ; mov ax, dx
    ; call print_number




    ; mov bl, 1
    ; mov ah, 0
    ; call card_picked
    ; mov bl, 2
    ; mov ah, 0
    ; call card_picked


    ; mov dx, offset newline
    ; mov ah, 09h
    ; int 21h

    ; CALL print_board
    ; CALL print_hand  

    ; mov dx, offset newline
    ; mov ah, 09h
    ; int 21h


    ; mov al, 1
    ; call calc_row
    ; mov ax, dx
    ; call print_number
    
    ; mov al, 2
    ; call calc_row
    ; mov ax, dx
    ; call print_number
    
    ; mov al, 3
    ; call calc_row
    ; mov ax, dx
    ; call print_number
    
    


    ; Main Logic:
    

    MOV AX, 4C00h      
    INT 21h
    MAIN ENDP


start_game PROC 

    newRound:

        call NewRoundPrep

        cmp [ComputerWins], 2
        je ComputerWon
        cmp [PlayerWins], 2
        je Skip20
            jmp game_continue
        Skip20:
            MOV [byte ptr offset filename], 'T'
            MOV [byte ptr offset filename+1], 'W'
            MOV [byte ptr offset filename+2], 'o'
            MOV [byte ptr offset filename+3], 'n'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 24
            MOV [imageLength], 156
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 106
            MOV [ppy], 32
            CALL PrintImage
        jmp gameEnd
        
        ComputerWon:
        cmp [PlayerWins], 2
        je Tie
            MOV [byte ptr offset filename], 'T'
            MOV [byte ptr offset filename+1], 'L'
            MOV [byte ptr offset filename+2], 'o'
            MOV [byte ptr offset filename+3], 'S'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 24
            MOV [imageLength], 156
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 106
            MOV [ppy], 32
            CALL PrintImage
        jmp gameEnd
        Tie:
            MOV [byte ptr offset filename], 'T'
            MOV [byte ptr offset filename+1], 'T'
            MOV [byte ptr offset filename+2], 'i'
            MOV [byte ptr offset filename+3], 'e'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 24
            MOV [imageLength], 156
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 106
            MOV [ppy], 32
            CALL PrintImage
        jmp gameEnd


        game_continue:
            call start_round

            MOV [byte ptr offset filename], 'S' ; space too end
            MOV [byte ptr offset filename+1], 'e'
            MOV [byte ptr offset filename+2], 'n'
            MOV [byte ptr offset filename+3], 'd'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 24
            MOV [imageLength], 156
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 121
            MOV [ppy], 52
            CALL PrintImage


            pressWait:
                    mov ah, 00h
                    int 16h
                    cmp al, ' '
                    jne pressWait
                jne pressWait
            jmp newRound


    gameEnd:
            MOV [byte ptr offset filename], 'R' ; r to end
            MOV [byte ptr offset filename+1], 'e'
            MOV [byte ptr offset filename+2], 'n'
            MOV [byte ptr offset filename+3], 'd'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            PUSH offset filename
            MOV [imageHeight], 24
            MOV [imageLength], 156
            PUSH [imageLength]
            PUSH [imageHeight]
            MOV [ppx], 121
            MOV [ppy], 52
            CALL PrintImage
        

        gameEndHelp:
        mov al, 0
        MOV AH, 00h
        INT 16h
        cmp AL, 114
        jne MenuStartHelper
        jmp MenuStart

        MenuStartHelper:
            jmp MenuScreen
        ret
start_game ENDP 

NewRoundPrep PROC
    push di
    
    mov [ComputerGiveUp], 0
    mov [PlayerGiveUp], 0

    xor di, di
    LoopStartNRP1:
        mov [lens + di], 0
        inc di
        cmp di, 6
        jl LoopStartNRP1


    xor di, di
    LoopStartNRP2:
        mov [board+di], 0
        inc di
        cmp di, 72
        jl LoopStartNRP2
    pop di


        MOV [byte ptr offset filename], 'b'
        MOV [byte ptr offset filename+1], 'o'
        MOV [byte ptr offset filename+2], 'a'
        MOV [byte ptr offset filename+3], 'r'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 200
        MOV [imageLength], 320
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 0
        CALL PrintImage

        CALL PrintHand

        call PrintGameStatus

    ret
NewRoundPrep ENDP

start_round PROC
    anotherPlay:
    ; call print_board
    ; call print_hand
    ; mov [ComputerGiveUp], 1
    mov al, [ComputerGiveUp]
    and al, [PlayerGiveUp]
    cmp al, 1

    jne JmpHelp007
    jmp roundEnds
    JmpHelp007:

    cmp [PlayerGiveUp], 1
    jne JmpHelp008
    jmp computerPlays
    JmpHelp008:

    cmp [ComputerGiveUp], 1
    jne JmpHelp009
    jmp playerPlays
    JmpHelp009:

    cmp [playerTurn], 1
    je JmpHelp010
    jmp computerPlays
    JmpHelp010:
    playerPlays:
    ; check if all cards are 0, if they are, playerGiveUp = 1
    mov si, offset hand ; functions exactly as doing the [] thing but looks cooler :D
    mov cx, 20

    check_loop:
        cmp byte ptr [si], 0
        jne hadCardsInHand
        inc si
        loop check_loop
    mov [PlayerGiveUp], 1
    jmp anotherPlay
    hadCardsInHand:

    wait_for_key:
    mov al, 0
    MOV AH, 00h
    INT 16h

    ; add that if \ is pressed than give up
    cmp AL, 45
    jne didnt_gave_up
        mov [PlayerGiveUp], 1
        jmp anotherPlay
    didnt_gave_up:
    cmp AL, 27
    jne keep_playing
    mov ah, 4ch
    int 21h
    keep_playing:


    CMP AL, '0'
    JL wait_for_key
    CMP AL, '9'
    JG wait_for_key


    sub al, '0'

    mov ah, 0
    mov bl, 2
    mul bl
    mov si, ax
    mov [placeHolder2], si
    mov al, [hand+si]
    inc si
    mov ah, [hand+si]
    mov bl, 1
    push si
    call place_card
    pop si
    cmp al, 101
    jne cardPlaced
    jmp wait_for_key
    cardPlaced:
    cmp [placeHolder2], 303
    je dontRemoveCard
    mov [hand+si], 0
    dec si
    mov [hand+si], 0

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    call PrintHand
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    dontRemoveCard:


    mov playerTurn, 0
    jmp anotherPlay
    computerPlays:
        call rng_1_to_3
        mov [sleepTime], al
        call Sleep
        call computerMove
        ; xor playerTurn, 1
        jmp anotherPlay
    roundEnds:
        ; compare players total score and computers total score, add a win to the winner
        mov al, 1
        push bx
        call calc_row
        pop bx
        mov bx, dx
        mov al, 2
        push bx
        call calc_row
        pop bx
        add bx, dx
        mov al, 3
        push bx
        call calc_row
        pop bx
        add bx, dx
        
        mov al, 4
        push cx
        push bx
        call calc_row
        pop bx
        pop cx
        mov cx, dx
        mov al, 5
        push cx
        push bx
        call calc_row
        pop bx
        pop cx
        add cx, dx
        mov al, 6
        push cx
        push bx
        call calc_row
        pop bx
        pop cx
        add cx, dx

        cmp cx, bx
        je tie
        ja computerWon ; swapped randomly, dont know why, dont know if its needed but I swap them cuz whenever I win I loss...
            inc [ComputerWins]
            ret
        computerWon:
            inc [PlayerWins]
            ret
        tie:
            inc [ComputerWins]
            inc [PlayerWins]
            ret
        ; call print_board  dont think its needed
        ; call print_hand   dont think its needed
start_round ENDP





;al the card's type, ah the card's value, bl == 0 means its the computer's turn, bl == 1 means its your turn
;IMPORTANT: the card that is being placed is NOT taken from the hand, remove it manually
place_card PROC 
    ; for print
    call PrintFlagCheck
    mov [cardType], al


    xor bl, 1
    mov [placeHolder], bl
    xor bl, 1
    cmp al, 0
    jne realCard
        cmp ah, 0
        jne realCard
            mov al, 101
            ret
    realCard:
    cmp ah, 16
    jae Skip8
        jmp figher
    Skip8:
    je hornF
    ja weather
    hornF: ; very complicated logic, can have some bugs, check it later... seems like its working, copyed (with some modifications) to other placed
        mov bh, 0
        push ax
        mov ah, 0
        mov al, bl
        mov cl, 3
        mul cl
        mov si, ax
        pop ax
        mov bx, ax
        mov bh, 0
        add si, bx
        dec si
        mov ax, si
        mov bl, 12
        mul bl
        mov si, ax
        add si, 10
        cmp [board+si], 1
        jne canPlace
            mov al, 101 ; cant place
            ret
        canPlace:
        mov [board+si], 1
        ; original:
        ;   mov si, 3*bl+ al-1
        ;   mov [board+si*12+10], 1
        
        call UpdateBoardEffects
        ret
    weather:
        cmp ah, 20
        je sun
        push ax
        sub ah, 17
        push bx
        mov bh, 0
        mov bl, ah
        mov ax, bx
        mov bx, 12
        mul bx
        pop bx
        mov ah, 0
        add ax, 11
        mov si, ax
        mov [board+si], 1
        add si, 3*12
        mov [board+si], 1
        pop ax
        call UpdateBoardEffects
        ret
    sun: ; can be used as a loop
        mov [board+11], 0
        mov [board+12+11], 0
        mov [board+12*2+11], 0
        mov [board+12*3+11], 0
        mov [board+12*4+11], 0
        mov [board+12*5+11], 0
        ; mov [board+12*6+11], 0

        mov [isHand], 0
        mov [cardValue], 20
        mov [cardType], 1
        mov [cardPos], 1
        mov [cardRow], 1
        CALL Print_Card
        ret
    figher:
        push bx
        mov bx, ax
        mov ah, 0
        mov si, ax
        mov ax, bx
        pop bx
        
        cmp [lens+si], 10
        jb hasSpace
        mov al, 101 ; means there is no space
        ret

        hasSpace:
        cmp ah, 10
        jbe normalCard
        cmp ah, 11
        je spyCard
        cmp ah, 12
        je spyCard
        cmp ah, 13
        jne dragonCardSkip
            jmp dragonCard
        dragonCardSkip:
        normalCard:
            ;mov bh, 0
            ;push ax
            ;mov ah, 0
            ;mov al, bl
            ;mov cl, 3
            ;mul cl
            ;mov si, ax
            ;pop ax
            ;mov bx, ax
            ;mov bh, 0
            ;add si, bx
            ;dec si
            ;mov ax, si
            ;mov bl, 12
            ;mul bl
            ;mov si, ax
            ;add si, 10

            push ax
            mov cx, bx
            mov ch, 0
            mov ax, cx
            mov cx, 3
            mul cx
            mov bx, ax
            pop ax
            mov cx, ax
            mov ch, 0
            add bx, cx
            dec bx
            mov si, bx

            mov cl, [lens+si] ; row length

            ;print logic
            push bx
            mov bl, cl
            mov bh, 0
            mov [cardRow], si
            mov [cardPos], bx
            pop bx

            add [lens+ si], 1
            push bx
            push ax
            mov ax, si
            mov bx, 12
            mul bx
            mov bx, ax
            add bl, cl
            
            pop ax
            ;mov bx, si*12+cx ; last place in row relativly to board
            mov [board+bx], ah
            pop bx
            
            ;print logic
            mov [isHand], 0
            mov [cardValue], ah
            call Print_Card
            ; cmp playerTurn, 0
            ; je SkipPrint1
            ; call PrintHand
            ; SkipPrint1:
            ret
        spyCard:
            push ax
            mov cx, bx
            mov ch, 0
            mov ax, cx
            mov cx, 3
            mul cx
            mov bx, 3
            sub bx, ax
            pop ax
            mov cx, ax
            mov ch, 0
            add bx, cx
            dec bx
            mov si, bx
            ;mov si, 3-bl*3 +al-1

            mov cl, [lens + si] ; row length

            ;print logic
            push bx
            mov bl, cl
            mov bh, 0
            mov [cardRow], si
            mov [cardPos], bx
            pop bx

            add [lens + si], 1
            push bx
            push ax
            mov ax, si
            mov bx, 12
            mul bx
            mov si, ax
            mov ax, cx
            mov ah, 0
            add si, ax
            mov bx, si
            pop ax
            ;mov bx, si*12+cx ; last place in row relativly to board
            mov [board+bx], ah
            pop bx
            
            ;print logic
            mov [isHand], 0
            mov [cardValue], ah
            call Print_Card
            ; cmp playerTurn, 0
            ; je SkipPrint3
            ; call PrintHand
            ; SkipPrint3:
            
            mov bl, [placeHolder]
            cmp bl, 1
            je computerSpy
            
            push si
            mov si, [placeHolder2]
            mov [hand+si], 0
            inc si
            mov [hand+si], 0
            mov [placeHolder2], 303
            pop si

            mov bx, 0
            mov cx, 0   
            spyLoop:  
                mov di, cx
                cmp [hand+di], 0
                jne notEmpty
                cmp [hand+di+1], 0
                jne notEmpty

                push bx
                push cx
                CALL rng_1_to_3
                mov [hand+di], al
                CALL rng_1_to_20
                mov [hand+di+1], al
                pop cx
                pop bx

                inc bx
                notEmpty:
                    add cx, 2
                    cmp bx, 2
                    je spyEnd
                    cmp cx, 20
                    jbe spyLoop

            spyEnd:
                ret
            computerSpy:
                add computerMoves, 2
                ret
        dragonCard:
            push bx
            push ax
            mov cx, bx
            mov ch, 0
            mov ax, cx
            mov cx, 3
            mul cx
            mov bx, ax
            pop ax
            mov cx, ax
            mov ch, 0
            add bx, cx
            dec bx
            mov si, bx
            pop bx
            ;mov si, bl*3 +al-1

            mov cl, [lens + si]
            
            ;print logic
            push bx
            mov bl, cl
            mov bh, 0
            mov [cardRow], si
            mov [cardPos], bx
            pop bx

            add [lens + si], 1
            push bx
            push ax
            mov ax, si
            mov bx, 12
            mul bx
            mov si, ax
            mov ax, cx
            mov ah, 0
            add si, ax
            mov bx, si
            pop ax
            ;mov bx, si*12+cx ; last place in row relativly to board
            mov [board+bx], ah
            pop bx
            mov [cardValue], ah
            mov [isHand], 0

            push ax
            push bx
            push cx
            push dx
            push si
            call Print_Card
            ; cmp playerTurn, 0
            ; je SkipPrint2
            ; call PrintHand
            ; SkipPrint2:
            pop si
            pop dx
            pop cx
            pop bx
            pop ax

            mov dl, 1
            sub dl, bl
            mov bl, dl
            mov bh, 0

            push ax
            mov cx, bx
            mov ch, 0
            mov ax, cx
            mov cx, 3
            mul cx
            mov bx, ax
            pop ax
            mov cx, ax
            mov ch, 0
            add bx, cx
            dec bx
            mov si, bx

            cmp [lens + si], 0
            je dragonEnd

            mov [cardRow], si
            push ax
            mov al, [lens+si]
            mov ah, 0
            dec ax
            mov [cardPos], ax
            pop ax

            dec [lens + si]
            mov cl, [lens + si]
            
            push bx
            push ax
            mov ax, si
            mov bx, 12
            mul bx
            mov si, ax
            mov ax, cx
            mov ah, 0
            add si, ax
            mov bx, si
            pop ax
            mov [board+bx], 0
            pop bx

            mov [isHand], 0
            mov [setName], 1

            MOV [byte ptr offset filename], 'R'
            MOV [byte ptr offset filename+1], 'a'
            MOV [byte ptr offset filename+2], 'r'
            MOV [byte ptr offset filename+3], 'd'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'

            mov [cardDead], 1            
            call Print_Card

            dragonEnd:
                ret
            
place_card ENDP


seed_random PROC ;generates random seed to begin the seed cycle with
    MOV AH, 00h      
    INT 1Ah        

    XOR seed, DX      
    XOR seed, CX       
    ;mov seed, 123
    RET
seed_random ENDP

fill_array PROC
    MOV SI, 0     
fill_loop:
    CALL rng_1_to_3
    MOV [hand + SI], AL
    CALL rng_1_to_20
    MOV [hand + SI+1], AL
    INC SI
    INC SI
    CMP SI, 20       
    JL fill_loop   
    RET
fill_array ENDP



rng_1_to_20 PROC
    rngstart20:
    mov ax, seed
    mov bx, 17
    mul bx
    add ax, 43
    mov cx, 256
    mov dx, 0
    div cx
    mov ax, dx
    mov seed, ax
    mov cx, 20
    mov dx, 0
    div cx
    mov al, dl
    add al, 1 
    cmp al, 0
    je rngstart20

    RET
rng_1_to_20 ENDP

rng_1_to_3 PROC
    rngstart3:
    mov ax, seed
    mov bx, 17
    mul bx
    add ax, 43
    mov cx, 256
    mov dx, 0
    div cx
    mov ax, dx
    mov seed, ax
    mov cx, 3
    mov dx, 0
    div cx
    mov al, dl
    add al, 1 
    cmp al, 0
    je rngstart3

    RET
rng_1_to_3 ENDP


card_picked PROC ; bl is the card number in hand 1-10, bh = 0  means player plays, bh = 1 means computer is playing

    cmp bh, 1
    je computerTurn

    dec bl
    mov al, bl
    mov ah, 0
    mov cx, 2
    mul cx
    inc al
    mov si, ax
    cmp [hand+si], 0
    je card_picked_end

    mov cl, bl
    mov ch, 0
    mov di, cx
    cmp [lens+di], 10
    jae card_picked_end

    
    mov ah, [hand+si]
    dec si
    mov al, [hand+si]
    mov bl, [playerTurn]

    mov [hand+si], 0
    inc si
    mov [hand+si], 0
    CALL place_card


    jmp card_picked_end

    computerTurn:
        ; if score player > score computer + 19 || computer cards + 2 < player cards && computer score > player score + 9
        ; give up

        ; if not give up
        dec computerMoves
        

    mov al, 1
    sub al, [playerTurn]
    mov [playerTurn], al

    card_picked_end:
        ret
card_picked ENDP

; al is row number 1-6
calc_row PROC
    mov dx, 0 ; total
    dec al
    mov ah, 0
    mov cx, 12
    mul cx

    mov cx, 0
    calc_row_loop:
        mov si, ax
        add si, cx
        cmp [board+si], 0
        jne calc_row_not_end
        jmp calc_row_end
        calc_row_not_end:
        cmp [board+si], 14
        je calc_row_legendary10
        cmp [board+si], 15
        je calc_row_legendary15

        mov di, ax
        add di, 11
        cmp [board+di], 1
        je calc_row_hurted
        
        cmp [board+si], 11
        je calc_row_spy1
        cmp [board+si], 12
        je calc_row_spy5
        cmp [board+si], 13
        je calc_row_dragon

        ;normal cards:
            mov bl, [board+si]
            jmp calc_row_horn_add

        calc_row_dragon:
            mov bl, 5
            jmp calc_row_horn_add
        calc_row_spy1:
            mov bl, 1
            jmp calc_row_horn_add

        calc_row_spy5:
            mov bl, 5
            jmp calc_row_horn_add

        calc_row_hurted:
            mov bl, 1
            jmp calc_row_horn_add
        
        calc_row_legendary10:
            add dx, 10
            jmp calc_row_loop_continue

        calc_row_legendary15:
            mov bl, 15 ; not needed but I aint gonna touch it :)
            mov bl, 10 ; not needed but I aint gonna touch it :)
            add dx, 15
            jmp calc_row_loop_continue


        calc_row_horn_add: ; takes bl
            mov di, ax
            add di, 10
            cmp [board+di], 1
            jne horn_add_end
            push ax
            mov al, bl
            mov ah, 0
            mov bl, 2
            mul bl
            mov bl, al
            pop ax
            horn_add_end:
                push cx
                mov cl, bl
                mov ch, 0
                add dx, cx
                pop cx
            jmp calc_row_loop_continue

        calc_row_loop_continue:
            inc cx
            cmp cx, 10
            je calc_row_end
            jmp calc_row_loop

    calc_row_end:
    ret

calc_row ENDP

calc_rowNL PROC ; calc_row but without legendaries
    push di
    push bx
    push cx
    push si
    mov bl, ah
    mov dx, 0 ; total
    dec al
    mov ah, 0
    mov cx, 12
    mul cx

    mov cx, 0
    calc_rowNL_loop:
        mov si, ax
        add si, cx
        cmp [board+si], 0
        je calc_rowNL_end
        cmp [board+si], 14
        je calc_rowNL_legendary10
        cmp [board+si], 15
        je calc_rowNL_legendary15

        mov di, ax
        add di, 11
        cmp bl, 1
        je NotHurted
        cmp [board+di], 1
        je calc_rowNL_hurted
        
        NotHurted:

        cmp [board+si], 11
        je calc_rowNL_spy1
        cmp [board+si], 12
        je calc_rowNL_spy5
        
        ;normal cards:
            mov bl, [board+si]
            jmp calc_rowNL_horn_add


        calc_rowNL_spy1:
            mov bl, 1
            jmp calc_rowNL_horn_add

        calc_rowNL_spy5:
            mov bl, 5
            jmp calc_rowNL_horn_add

        calc_rowNL_hurted:
            mov bl, 1
            jmp calc_rowNL_horn_add
        
        calc_rowNL_legendary10:
            jmp calc_rowNL_loop_continue

        calc_rowNL_legendary15:
            jmp calc_rowNL_loop_continue


        calc_rowNL_horn_add: ; takes bl
            mov di, ax
            add di, 10
            cmp [board+di], 1
            jne horn_add_endNL
            push ax
            mov al, bl
            mov ah, 0
            mov bl, 2
            mul bl
            mov bl, al
            pop ax
            horn_add_endNL:
                push cx
                mov cl, bl
                mov ch, 0
                add dx, cx
                pop cx
            jmp calc_rowNL_loop_continue

        calc_rowNL_loop_continue:
            inc cx
            cmp cx, 10
            je calc_rowNL_end
            jmp calc_rowNL_loop

    calc_rowNL_end:
    pop si
    pop cx
    pop bx
    pop di
    ret

calc_rowNL ENDP

computerHand PROC
    mov cx, 0
    computerHand_loop:
        add [computerMoves], 1
        push cx
        CALL rng_1_to_20
        pop cx
        cmp al, 3
        jbe computerHand_weather
        cmp al, 4
        jbe computerHand_horn

        jmp computerHandEnd

        computerHand_weather:
            push si
            push cx
            mov cl, [cwl]
            mov ch, 0
            mov si, cx
            pop cx
            mov [computerWeather+si], al
            pop si
            inc cwl
            jmp computerHandEnd

        computerHand_horn:
            push cx
            CALL rng_1_to_3
            pop cx
            push si
            push cx
            mov cl, [chl]
            mov ch, 0
            mov si, cx
            pop cx
            mov [computerHorn+si], al
            pop si
            inc chl
            jmp computerHandEnd

        computerHandEnd:
            inc cx
            cmp cx, 10
            jb computerHand_loop
            ; push ax
            ; mov al, [cwl]
            ; add al, [chl]
            ; add al, [csl]
            ; mov [computerMoves], al
            ; pop ax
            ret 
computerHand ENDP


computerMove PROC
    ; Cscore > Pscore+17
    cmp [computerMoves], 0
    jne computerHasCards
        jmp computerMoveGU
    computerHasCards:
    mov al, 4
    push bx
    call calc_row
    pop bx
    mov bx, dx
    mov al, 5
    push bx
    call calc_row
    pop bx
    add bx, dx
    mov al, 6
    push bx
    call calc_row
    pop bx
    add bx, dx
    
    mov al, 1
    push cx
    push bx
    call calc_row
    pop bx
    pop cx
    mov cx, dx
    mov al, 2
    push cx
    push bx
    call calc_row
    pop bx
    pop cx
    add cx, dx
    mov al, 3
    push cx
    push bx
    call calc_row
    pop bx
    pop cx
    add cx, dx

    cmp PlayerGiveUp, 1
    je TryToWin
    add cx, 17
    cmp cx, bx
    ja JmpHelp006
    cmp PlayerWins, 1
    je JmpHelp006
    jmp computerMoveGU
    JmpHelp006:
    sub cx, 17
    ;sub cx, 18
    add cx, 18

    cmp cx, bx
    jnb JmpHelp005
    jmp computerMoveGU
    JmpHelp005:

    jmp NotTryToWin

    TryToWin:
    cmp cx, bx
    jbe JmpHelp011
    jmp computerMoveGU
    JmpHelp011:
    
    NotTryToWin:

    mov al, [cwl]
    add al, [chl]
    add al, [csl]
    mov bl, [computerMoves]
    sub bl, al
    mov bh, 0
    cmp bx, 0

    je computerMoveWeather

    cmp ax, 0
    jne JmpHelp004
    jmp NoWeather
    JmpHelp004:
    
    jmp computerMoveWeatherOptional    
    computerMoveWeather:

        cmp [csl], 0
        jne JmpHelp003
        jmp No_Sunshine
        JmpHelp003:
        mov bx, 0

        mov ah, 1
        mov al, 1
        call calc_rowNL
        mov al, 1
        sub al, [board+11]
        mul dl
        add bx, ax
        
        mov ah, 1
        mov al, 2
        call calc_rowNL
        mov al, 1
        sub al, [board+12+11]
        mul dl
        add bx, ax

        mov ah, 1
        mov al, 3
        call calc_rowNL
        mov al, 1
        sub al, [board+12*2+11]
        mul dl
        add bx, ax

        mov ah, 1
        mov al, 4
        call calc_rowNL
        mov al, 1
        sub al, [board+12*3+11]
        mul dl
        add cx, ax

        mov ah, 1
        mov al, 5
        call calc_rowNL
        mov al, 1
        sub al, [board+12*4+11]
        mul dl
        add cx, ax

        mov ah, 1
        mov al, 6
        call calc_rowNL
        mov al, 1
        sub al, [board+12*5+11]
        mul dl
        add cx, ax

        ; I may or may not have mistaken the registers so imma just do a swap
        push dx
        mov dx, cx
        mov cx, bx
        mov bx, dx
        pop dx

        ; add bx, 8
        cmp cx, bx
        jbe No_Sunshine

        mov al, 2
        mov ah, 20
        mov bl, 0
        call place_card
        dec [csl]
        dec [computerMoves]
        jmp computerMoveEnd

        No_Sunshine:

        cmp [cwl], 0
        je computerMoveHorn
        mov cx, 0
        push cx
        mov cl, [cwl]
        mov ch, 0
        mov di, cx
        pop cx
        computerMoveWeather_loop:
            cmp di, cx ; might be di,cx +1
            je computerMoveHorn
            inc cx

            mov ah, 0
            mov si, cx
            dec si
            mov al, [computerWeather+si]
            call calc_rowNL
            mov bx, dx
            mov al, [computerWeather+si]
            add al, 3
            push bx
            call calc_rowNL
            pop bx

            ; add dx, 10         Because other option is give up
            cmp bx, dx ; generally good, but in some cases can lead to self hurm...
            jae computerMoveWeather_loop

            mov al, [computerWeather+si]
            mov ah, 16
            add ah, [computerWeather+si]
            mov bl, 0
            push si
            call place_card
            pop si
            mov [computerWeather+si], 0
            dec [cwl]
            dec [computerMoves]
            jmp computerMoveEnd

        computerMoveHorn:

            ; horn logic should be implemented here, check if horned already
            
        cmp [chl], 0
        jne JmpHelp002
        jmp computerMoveGU
        JmpHelp002:
        mov cx, 0
        push cx
        mov cl, [chl]
        mov ch, 0
        mov di, cx
        pop cx
        computerMoveHorn_loop:
            cmp di, cx ; might be di\cx +1
            jne JmpHelp001
            jmp computerMoveGU
            JmpHelp001:

            mov si, cx 
            ; inc cx    ; This logic should be used when we can decide not to place it
            ; dec si
            ; mov al, [chl+si]
            ; call calc_rowNL
            ; mov bx, dx
            ; mov al, [chl+si]
            ; add al, 3
            ; push bx
            ; call calc_rowNL
            ; pop bx

            ; mov bx, 10
            ; cmp dx, bx
            ; jbe computerMoveHorn_loop 

            mov al, [computerHorn+si]
            mov ah, 16
            mov bl, 0
            push si
            call place_card
            pop si
            mov [computerHorn+si], 0
            dec [chl]
            dec [computerMoves]
            jmp computerMoveEnd




    computerMoveWeatherOptional:

        cmp [csl], 0
        jne No_SunShineOptionalH
        jmp No_SunshineOptional
        No_SunShineOptionalH:
        mov bx, 0
        
        mov ah, 1
        mov al, 1
        call calc_rowNL
        mov al, 1
        sub al, [board+11]
        mul dl
        add bx, ax
        
        mov ah, 1
        mov al, 2
        call calc_rowNL
        mov al, 1
        sub al, [board+12+11]
        mul dl
        add bx, ax

        mov ah, 1
        mov al, 3
        call calc_rowNL
        mov al, 1
        sub al, [board+12*2+11]
        mul dl
        add bx, ax

        mov ah, 1
        mov al, 4
        call calc_rowNL
        mov al, 1
        sub al, [board+12*3+11]
        mul dl
        add cx, ax

        mov ah, 1
        mov al, 5
        call calc_rowNL
        mov al, 1
        sub al, [board+12*4+11]
        mul dl
        add cx, ax

        mov ah, 1
        mov al, 6
        call calc_rowNL
        mov al, 1
        sub al, [board+12*5+11]
        mul dl
        add cx, ax

        sub bx, 8
        cmp bx, cx
        jbe No_SunshineOptional

        mov al, 2
        mov ah, 20
        mov bl, 0
        call place_card
        dec [csl]
        dec [computerMoves]
        jmp computerMoveEnd

        No_SunshineOptional:
        cmp [cwl], 0
        je computerMoveHornOptional
        
        mov cx, 0
        push cx
        mov cl, [cwl] ; changed 3/4
        mov ch, 0
        mov di, cx
        pop cx
        computerMoveWeatherOptional_loop:
            cmp di, cx ; might be di\cx +1
            je computerMoveHornOptional
            inc cx

            mov ah, 0
            mov si, cx
            dec si
            mov al, [computerWeather+si]
            call calc_rowNL
            mov bx, dx
            mov al, [computerWeather+si]
            add al, 3
            push bx
            call calc_rowNL
            pop bx

            add bx, 6         ; Because other option is give up
            cmp bx, dx ; generally good, but in some cases can lead to self hurm...
            jae computerMoveWeatherOptional_loop

            mov al, [computerWeather+si]
            mov ah, 16
            add ah, [computerWeather+si]
            mov bl, 0
            push si
            call place_card
            pop si
            mov [computerWeather+si], 0
            dec [cwl]
            dec [computerMoves]
            jmp computerMoveEnd

        computerMoveHornOptional:

            ; horn logic should be implemented here, check if horned already
            
        cmp [chl], 0
        je NoWeather
        mov cx, 0
        push cx
        mov cl, [chl]
        mov ch, 0
        mov di, cx
        pop cx
        computerMoveHornOptional_loop:
            cmp di, cx ; might be di\cx +1
            je NoWeather

            mov ah, 0
            mov si, cx 
            inc cx
            dec si
            mov al, [computerHorn+si]
            call calc_rowNL
            mov bx, dx
            ; add al, 3
            ; mov al, [chl+si]
            ; push bx
            ; call calc_rowNL
            ; pop bx

            ; mov bx, 6
            ; cmp dx, bx
            cmp bx, 8
            jbe computerMoveHornOptional_loop 

            mov al, [computerHorn+si]
            mov ah, 16
            mov bl, 0
            push si
            call place_card
            pop si
            mov [computerHorn+si], 0
            dec [chl]
            dec [computerMoves]
            jmp computerMoveEnd


    NoWeather:
        call rng_1_to_20   
        cmp al, 15
        ja NoWeather
        mov ch, al
        NoWeather3:
        push cx
        call rng_1_to_3
        pop cx
        mov ah, 0
        mov si, ax
        ; [lens+3+si]
        ; add si, 3 wrong calc PROBABLY so I removed it
        cmp [lens+si], 10
        je NoWeather3
        mov ah, ch
        ;mov ax, cx
        mov bl, 0
        call place_card
        dec [computerMoves]
        jmp computerMoveEnd
    computerMoveGU:
        mov [ComputerGiveUp], 1
        jmp computerMoveEnd
    
    computerMoveEnd:
        mov [playerTurn], 1
        ret
computerMove ENDP

print_hand PROC
    MOV SI, 0        
    MOV CX, 0 

print_loop:
    MOV AL, [hand + SI]
    CALL print_number  

    MOV DX, OFFSET separator  
    MOV AH, 09h      
    INT 21h

    INC SI
    MOV AL, [hand + SI]
    CALL print_number  

    INC SI
    INC CX



    CMP CX, 1
    JL continue_loop

    MOV DX, OFFSET bar_separator
    MOV AH, 09h
    INT 21h

    MOV CX, 0

continue_loop:
    CMP SI, 20
    JL print_loop    

    MOV DX, OFFSET newline
    MOV AH, 09h      
    INT 21h

    RET
print_hand ENDP

; print_number PROC
;     CMP AL, 10
;     JL single_digit

;     MOV AH, 0
;     MOV BL, 10
;     DIV BL

;     push ax
;     ADD AL, '0'
;     MOV DL, AL
;     MOV AH, 02h
;     INT 21h
;     pop ax

;     MOV AL, AH    
;     ADD AL, '0'   
;     MOV DL, AL
;     MOV AH, 02h
;     INT 21h
;     RET

; single_digit:
;     ADD AL, '0'   
;     MOV DL, AL
;     MOV AH, 02h
;     INT 21h
;     RET
; print_number ENDP



; gets value from al, might just need to change to ax (for larger numbers)
print_number PROC
    push di
    push bx
    CMP AL, 10
    JL single_digit

    MOV AH, 0
    MOV BL, 10
    DIV BL

    ADD AL, '0'
    MOV DL, AL
    push ax
    MOV AH, 02h
    INT 21h
    pop ax

    MOV AL, AH    
    ADD AL, '0'   
    MOV DL, AL
    MOV AH, 02h
    INT 21h
    pop bx
    pop di
    RET

single_digit:
    ADD AL, '0'   
    MOV DL, AL
    MOV AH, 02h
    INT 21h
    pop bx
    pop di
    RET
print_number ENDP



print_board PROC
    MOV DI, 0
    MOV BX, 0    
    
print_row:
    push ax
    mov ax, bx
    mov ah, 0
    inc al
    push bx
    push di
    call calc_row
    mov ax, dx
    call print_number
    pop di
    pop bx
    pop ax


    MOV DX, OFFSET space  
    MOV AH, 09h      
    INT 21h
    INT 21h


    MOV SI, 0      
    MOV CX, 0     


print_loop_:
    push bx
    mov bx, 0
    add bx, DI
    add bx, SI
    MOV AL, [board + bx]
    pop bx
    CALL print_number  

    MOV DX, OFFSET separator  
    MOV AH, 09h      
    INT 21h

    INC SI
    push bx
    mov bx, 0
    add bx, DI
    add bx, SI
    MOV AL, [board + bx]
    pop bx
    CALL print_number  

    INC SI
    INC CX

    CMP CX, 1
    JL continue_loop_

    ;MOV DX, OFFSET bar_separator
    MOV DX, OFFSET separator  
    MOV AH, 09h
    INT 21h


    MOV CX, 0 

continue_loop_:
    CMP SI, 12
    JL print_loop_   

    MOV DX, OFFSET newline
    MOV AH, 09h      
    INT 21h

    ADD DI, 12   
    INC BX

    CMP BX, 3
    JNE check_rows
    

    push bx
    push di
    mov al, [ComputerGiveUp]
    call print_number

    mov al, [computerMoves]
    call print_number

    mov al, [cwl]
    call print_number

    mov al, [chl]
    call print_number

    mov al, [csl]
    call print_number
    pop di
    pop bx

    MOV DX, OFFSET newline
    MOV AH, 09h
    INT 21h
check_rows:
    CMP BX, 6     
    je dont_print_row
    jmp print_row    
    dont_print_row:
    RET
print_board ENDP


PrintImage proc ;Prints an image, considering the size and name of the file inputed.
push bp
mov bp, sp
nameOffset equ [bp+8]
imHeight equ [bp+4]
imLength equ [bp+6]

call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap
call CloseFile

pop bp
ret 6
PrintImage endp
OpenFile proc;Opens a file for reading.
;push bp
;mov bp, sp

; Open file
mov ah, 3Dh
xor al, al ; al =0, hence we will only be able to read from the file.
mov dx, [bp+8]
int 21h
jc openerror
mov [filehandle], ax
;pop bp
ret 

openerror:
mov dx, offset ErrorMsg ;dx = offset ErrorMsg in order to print "Error" to the screen.
mov ah, 9h
int 21h
ret 
OpenFile endp 
ReadHeader proc  
; Read BMP file header, 54 bytes
mov ah,3fh
mov bx, [filehandle]
mov cx,54
mov dx,offset Header
int 21h
ret 
ReadHeader endp 
ReadPalette proc

; Read BMP file color palette, 256 colors * 4 bytes (400h)
mov ah,3fh
mov cx,400h
mov dx,offset Palette
int 21h
ret 
ReadPalette endp
CopyPal proc
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
mov si,offset Palette
mov cx,256
mov dx,3C8h
mov al,0
; Copy starting color to port 3C8h
out dx,al
; Copy palette itself to port 3C9h
inc dx
PalLoop:
; Note: Colors in a BMP file are saved as BGR values rather than RGB.
mov al,[si+2] ; Get red value.
shr al,2 ; Max. is 255, but video palette maximal
; value is 63. Therefore dividing by 4.
out dx,al ; Send it.
mov al,[si+1] ; Get green value.
shr al,2
out dx,al ; Send it.
mov al,[si] ; Get blue value.
shr al,2
out dx,al ; Send it.
add si,4 ; Point to next color.
; (There is a null chr. after every color.)

loop PalLoop
ret 
CopyPal endp
CopyBitmap proc 
    mov ax, 0A000h  ; Set ES to video memory segment
    mov es, ax
    mov cx, imHeight ; Number of lines

	add cx, [ppy] ; change back to ppy ************
	PrintBMPLoop:
    push cx

    ; di = cx * 320, point to the correct screen line
    mov di, cx
    shl cx, 6
    shl di, 8
    add di, cx
	add di, [ppx] ; change back to ppx **************

    ; Read one line
    mov ah, 3Fh
    mov cx, imLength
    mov dx, offset ScrLine
    int 21h

    ; Copy one line into video memory with color check
    cld                ; Clear direction flag
    mov cx, imLength   ; Number of pixels in the line
    mov si, offset ScrLine

	CopyPixelLoop:
    lodsb              ; Load byte from DS:SI into AL (pixel color)
    cmp al, 253        ; Compare pixel color with color to skip (0Fh is an example)
    je SkipPixel       ; If color matches, skip writing

    stosb              ; Store AL at ES:DI (write pixel)
    jmp ContinueLoop

	SkipPixel:
    inc di             ; Move destination index forward (skip pixel)

	ContinueLoop:
    loop CopyPixelLoop ; Repeat for all pixels in the line

    pop cx
    loop PrintBMPLoop

    ret 
CopyBitmap endp
CloseFile proc;Closes a file.
mov ah,3Eh
mov bx, [filehandle]
int 21h
ret 
CloseFile endp 


; Print_Card proc
;         mov ax, 0
;         mov bx, 0
;         MOV [byte ptr offset filename], 'b'
;         MOV [byte ptr offset filename+1], 'o'
;         MOV [byte ptr offset filename+2], 'a'
;         MOV [byte ptr offset filename+3], 'r'
;         MOV [byte ptr offset filename+4], '.'
;         MOV [byte ptr offset filename+5], 'b'
;         MOV [byte ptr offset filename+6], 'm'
;         MOV [byte ptr offset filename+7], 'p'
;         PUSH offset filename
;         MOV [imageHeight], 200
;         MOV [imageLength], 320
;         PUSH [imageLength]
;         PUSH [imageHeight]
;         mov bx, [ph3]
;         mov ax, [ph4]
;         MOV [ppx], bx
;         MOV [ppy], ax
;         ;CALL PrintImage
;         pop ax
;         pop ax
;         pop ax
;         ret
; Print_Card endp

PrintFlagCheck proc

    push ax
    push bx
    push cx
    push dx
    push si
    push di

    cmp [ComputerGiveUp], 1
    jne Skip21

        MOV [byte ptr offset filename], 'f'
        MOV [byte ptr offset filename+1], 'l'
        MOV [byte ptr offset filename+2], 'a'
        MOV [byte ptr offset filename+3], 'g'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 36
        MOV [imageLength], 40
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 33
        CALL PrintImage

    Skip21:

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

        ret
PrintFlagCheck endp

Print_Card proc ; , cardType, cardValue,  int rowNumber (0-5), position (0-9), bool isHand
    cmp [cardRow], 2
    jne KeepRow
        mov [cardRow], 0
        jmp endSwap
    KeepRow:
    cmp [cardRow], 0
    jne endSwap
        mov [cardRow], 2
        jmp endSwap
    endSwap:


    mov ph3, 3 ; hieght
    mov ph4, 0
    mov bx, [isHand]
    cmp bx, 0
    je BoardPlace


    mov cx, [cardPos]
    ; dec cx
    mov ph3, 173
    mov ph4, 29
    HandLoop:
        cmp cx, 0
        je HandLoopEnd
        dec cx
        add ph4, 28
        jmp HandLoop
    HandLoopEnd:
    jmp PlaceCardOnScreen

    BoardPlace:

        mov bx, [cardPos]
        add ph4, 70
        WidthLoopPC:
            cmp bx, 0
            je WidthLoopPCExit
            dec bx
            add ph4, 24
            jmp WidthLoopPC
        WidthLoopPCExit:
        mov bx, [cardRow]
        ; dec bx
        cmp bx, 3
        jb upperLine
            add ph3, 87
            sub bx, 3
        upperLine:
        PlaceLoopPC:
            cmp bx, 0
            je PlaceLoopPCExit
            dec bx
            add ph3, 28
            jmp PlaceLoopPC
        PlaceLoopPCExit:

        ; push ax
        ; mov ax, 13h
        ; int 10h
        ; pop ax
        PlaceCardOnScreen:
        cmp [setName], 1
        jne Skip3
        jmp keepName
        Skip3:

        cmp [cardValue], 14
        je GoldenCard
        cmp [cardValue], 15
        je GoldenCard
        
        cmp [cardValue], 11
        je SpyCard
        cmp [cardValue], 12
        je SpyCard

        cmp [cardValue], 13
        je DragonCard

        cmp [cardValue], 17
        je FreezeCard

        cmp [cardValue], 18
        jne Skip16
            jmp RainCard
        Skip16:

        cmp [cardValue], 19
        jne Skip13
            jmp LightningCard
        Skip13:

        cmp [cardValue], 20
        jne Skip14
            jmp SunCard
        Skip14:

        cmp [cardValue], 16
        jne Skip15
            jmp HornCard
        Skip15:

        jmp NormalCard
        
        NormalCard:
            MOV [byte ptr offset filename], 'c'
            jmp DoneWithCardAbility
        SpyCard:
            MOV [byte ptr offset filename], 'S'
            jmp DoneWithCardAbility
        GoldenCard:
            MOV [byte ptr offset filename], 'G'
            jmp DoneWithCardAbility
        DragonCard:
            MOV [byte ptr offset filename], 'D'
            jmp DoneWithCardAbility
        DoneWithCardAbility:
            MOV [byte ptr offset filename+1], 'a'
            MOV [byte ptr offset filename+2], 'r'
            MOV [byte ptr offset filename+3], 'd'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
            jmp keepName

        FreezeCard:
        cmp [isHand], 1
        jne DontPrint
            ; Call UpdateBoardEffects
        MOV [byte ptr offset filename+3], 'F'
        mov [cardDead], 1
            jmp DoneWithCardWeather
        RainCard:
        cmp [isHand], 1
        jne DontPrint
            ; Call UpdateBoardEffects
        MOV [byte ptr offset filename+3], 'R'
        mov [cardDead], 1
            jmp DoneWithCardWeather
        LightningCard:
        cmp [isHand], 1
        jne DontPrint
            ; Call UpdateBoardEffects
        mov [cardDead], 1
        MOV [byte ptr offset filename+3], 'L'
            jmp DoneWithCardWeather
        HornCard:
        cmp [isHand], 1
        jne DontPrint
            ; Call UpdateBoardEffects
        MOV [byte ptr offset filename+3], 'H'
        mov [cardDead], 0
            jmp DoneWithCardWeather
            
        SunCard:
        cmp [isHand], 1
        jne SunUsed
        mov [cardDead], 1
            MOV [byte ptr offset filename+3], 'S'
                jmp DoneWithCardWeather
        SunUsed:
            call ClearWeather
            jmp Print_Card_End
        DontPrint:
            jmp Print_Card_End
        
        DoneWithCardWeather:
            MOV [byte ptr offset filename], 'W'
            MOV [byte ptr offset filename+1], 't'
            MOV [byte ptr offset filename+2], 'r'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'
        
        keepName:
        mov [setName], 0
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        mov bx, [ph4]
        mov ax, [ph3]
        MOV [ppx], bx
        MOV [ppy], ax
        CALL PrintImage

        cmp [cardDead], 1
        jne CardAlive
        jmp CardKilled
        CardAlive:
        cmp [cardType], 1
        je Type1Card
        cmp [cardType], 2
        je Type2Card
        cmp [cardType], 3
        je Type3Card
        jmp openerror

        Type1Card:
            MOV [byte ptr offset filename+3], '1'
            jmp DoneWithName
        Type2Card:
            MOV [byte ptr offset filename+3], '2'
            jmp DoneWithName
        Type3Card:
            MOV [byte ptr offset filename+3], '3'
            jmp DoneWithName
        
        DoneWithName:
            MOV [byte ptr offset filename], 't'
            MOV [byte ptr offset filename+1], 'y'
            MOV [byte ptr offset filename+2], 'p'
            MOV [byte ptr offset filename+4], '.'
            MOV [byte ptr offset filename+5], 'b'
            MOV [byte ptr offset filename+6], 'm'
            MOV [byte ptr offset filename+7], 'p'

        PUSH offset filename
        MOV [imageHeight], 12
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        inc [ph4]
        inc [ph3]
        mov bx, [ph4]
        mov ax, [ph3]
        MOV [ppx], bx
        MOV [ppy], ax
        CALL PrintImage


        cmp [cardValue], 16
        jne Skip19
            jmp Print_Card_End
        Skip19:

        MOV [byte ptr offset filename], 'n'
        MOV [byte ptr offset filename+1], 'u'
        MOV [byte ptr offset filename+2], 'm'
        MOV [byte ptr offset filename+3], '0' ; template
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'

        cmp [cardValue], 1
        je Value1Card
        cmp [cardValue], 2
        je Value2Card
        cmp [cardValue], 3
        je Value3Card
        cmp [cardValue], 4
        je Value4Card
        cmp [cardValue], 5
        je Value5Card
        cmp [cardValue], 6
        je Value6Card
        cmp [cardValue], 7
        je Value7Card
        cmp [cardValue], 8
        je Value8Card
        cmp [cardValue], 9
        je Value9Card

        cmp [cardValue], 10
        jne NValue10Card
        jmp Value10Card
        NValue10Card:
        cmp [cardValue], 11
        jne NValue11Card
        jmp Value11Card
        NValue11Card:
        cmp [cardValue], 12
        jne NValue12Card
        jmp Value12Card
        NValue12Card:
        cmp [cardValue], 13
        jne NValue13Card
        jmp Value13Card
        NValue13Card:
        cmp [cardValue], 14
        jne NValue14Card
        jmp Value14Card
        NValue14Card:
        cmp [cardValue], 15
        jne NValue15Card
        jmp Value15Card
        NValue15Card:
        jmp openerror

        Value1Card:
            MOV [byte ptr offset filename+3], '1'
            jmp DoneWithValue
        Value2Card:
            MOV [byte ptr offset filename+3], '2'
            jmp DoneWithValue
        Value3Card:
            MOV [byte ptr offset filename+3], '3'
            jmp DoneWithValue
        Value4Card:
            MOV [byte ptr offset filename+3], '4'
            jmp DoneWithValue
        Value5Card:
            MOV [byte ptr offset filename+3], '5'
            jmp DoneWithValue
        Value6Card:
            MOV [byte ptr offset filename+3], '6'
            jmp DoneWithValue
        Value7Card:
            MOV [byte ptr offset filename+3], '7'
            jmp DoneWithValue
        Value8Card:
            MOV [byte ptr offset filename+3], '8'
            jmp DoneWithValue
        Value9Card:
            MOV [byte ptr offset filename+3], '9'
            jmp DoneWithValue
        Value10Card:
            MOV [byte ptr offset filename+2], '1'
            MOV [byte ptr offset filename+3], '0'
            jmp DoneWithValue
        Value11Card:
            MOV [byte ptr offset filename+2], 'm'
            MOV [byte ptr offset filename+3], '1'
            jmp DoneWithValue
        Value12Card:
            MOV [byte ptr offset filename+2], 'm'
            MOV [byte ptr offset filename+3], '5'
            jmp DoneWithValue
        Value13Card:
            MOV [byte ptr offset filename+2], 'm'
            MOV [byte ptr offset filename+3], '5'
            jmp DoneWithValue
        Value14Card:
            MOV [byte ptr offset filename+2], '1'
            MOV [byte ptr offset filename+3], '0'
            jmp DoneWithValue
        Value15Card:
            MOV [byte ptr offset filename+2], '1'
            MOV [byte ptr offset filename+3], '5'
            jmp DoneWithValue

        DoneWithValue:


        PUSH offset filename
        MOV [imageHeight], 8
        MOV [imageLength], 8
        PUSH [imageLength]
        PUSH [imageHeight]
        inc [ph4]
        inc [ph3]
        mov bx, [ph4]
        mov ax, [ph3]
        add ax, 10
        MOV [ppx], bx
        MOV [ppy], ax
        CALL PrintImage

        CardKilled:
        mov [cardDead], 0

        cmp [isHand], 1
        je Skip4
        Call UpdateBoardEffects
        Skip4:

        Print_Card_End:
            call UpdateBoardEffects
        ret
Print_Card endp

UpdateBoardEffects proc ; Extremly long code that can be shortened tho it works :D

    cmp [board+11+12*2], 1
    jne KeepGoing1
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'L'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28
        CALL PrintImage
    KeepGoing1:

    cmp [board+11+12], 1
    jne KeepGoing2
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'R'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28*2
        CALL PrintImage
    KeepGoing2:

    cmp [board+11], 1
    jne KeepGoing3
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'F'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28*3
        CALL PrintImage
    KeepGoing3:

    cmp [board+11+12*3], 1
    jne KeepGoing4
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'F'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28*4
        CALL PrintImage
    KeepGoing4:

    cmp [board+11+12*4], 1
    jne KeepGoing5
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'R'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28*5
        CALL PrintImage
    KeepGoing5:

    cmp [board+11+12*5], 1
    jne KeepGoing6
        MOV [byte ptr offset filename], 'A'
        MOV [byte ptr offset filename+1], 'f'
        MOV [byte ptr offset filename+2], 'f'
        MOV [byte ptr offset filename+3], 'L'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 22
        MOV [imageLength], 244
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 69
        MOV [ppy], 172-28*6
        CALL PrintImage
    KeepGoing6:


        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'

    cmp [board+10+12*2], 1
    jne KeepGoing1H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28*6
        CALL PrintImage
    KeepGoing1H:

        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'


    cmp [board+10+12], 1
    jne KeepGoing2H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28*5
        CALL PrintImage
    KeepGoing2H:

        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'


    cmp [board+10], 1
    jne KeepGoing3H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28*4
        CALL PrintImage
    KeepGoing3H:

        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'


    cmp [board+10+12*3], 1
    jne KeepGoing4H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28*3
        CALL PrintImage
    KeepGoing4H:

        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'


    cmp [board+10+12*4], 1
    jne KeepGoing5H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28*2
        CALL PrintImage
    KeepGoing5H:

        MOV [byte ptr offset filename], 'W'
        MOV [byte ptr offset filename+1], 't'
        MOV [byte ptr offset filename+2], 'r'
        MOV [byte ptr offset filename+3], 'H'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'


    cmp [board+10+12*5], 1
    jne KeepGoing6H
        PUSH offset filename
        MOV [imageHeight], 20
        MOV [imageLength], 12
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 40
        MOV [ppy], 172-28
        CALL PrintImage
    KeepGoing6H:

    ret
UpdateBoardEffects endp

ClearWeather proc

        MOV [byte ptr offset filename], 'b'
        MOV [byte ptr offset filename+1], 'o'
        MOV [byte ptr offset filename+2], 'a'
        MOV [byte ptr offset filename+3], 'r'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 200
        MOV [imageLength], 320
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 0
        CALL PrintImage

        CALL PrintHand

        call PrintGameStatus

        xor bx, bx
        xor cx, cx
        xor dx, dx
        mov [isHand], 0
        PrintCardsLoop:

        mov [cardPos], cx
        mov [cardRow], dx

        mov di, bx
        xor ax, ax
        mov ah, [board+di]
        mov [cardValue], ah
        cmp [cardValue], 0
        je Skip9
        
        mov ax, dx
        cmp ax, 2
        jbe PosIsType
            sub ax, 3
        PosIsType:
        mov [cardType], al
        inc [cardType]

        push bx
        push cx
        push dx
        call Print_Card
        pop dx
        pop cx
        pop bx

        Skip9:
        inc cx
        cmp cx, 10
        jne Skip6
            inc bx
            inc bx
            xor cx, cx
            inc dx
        Skip6:
        inc bx
        cmp bx, 72
        jb PrintCardsLoop

            call PrintFlagCheck
    
        ret
ClearWeather endp

PrintHand PROC
    
        MOV [byte ptr offset filename], 'C'
        MOV [byte ptr offset filename+1], 'a'
        MOV [byte ptr offset filename+2], 'n'
        MOV [byte ptr offset filename+3], 'd'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'
        PUSH offset filename
        MOV [imageHeight], 24
        MOV [imageLength], 284
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 29
        MOV [ppy], 173
        CALL PrintImage


    MOV SI, 0     
PrintHandLoop:
    mov al, [hand+si]
    mov [cardType], al ; for print
    mov al, [hand+si+1]
    mov [cardValue], al ; for print
    cmp al, 0
    je SkipPrintCard
    
    ; for print
    push bx
    push ax
        mov ax, si
        mov bl, 2
        xor ah, ah
        div bl
        xor ah, ah
        mov [cardPos], ax
    pop ax
    pop bx
    mov [isHand], 1    
    
    push si
    call Print_Card
    pop si

    SkipPrintCard:

    INC SI
    INC SI
    CMP SI, 20       
    JL PrintHandLoop   
    RET
PrintHand ENDP

PrintGameStatus proc

        MOV [byte ptr offset filename+2], 's'
        MOV [byte ptr offset filename+3], 'c'
        MOV [byte ptr offset filename+4], '.'
        MOV [byte ptr offset filename+5], 'b'
        MOV [byte ptr offset filename+6], 'm'
        MOV [byte ptr offset filename+7], 'p'

        cmp [ComputerWins], 0
        jne PGSC0 ; print game status computer + value (0)
            MOV [byte ptr offset filename], '0'
        PGSC0:
        cmp [ComputerWins], 1
        jne PGSC1
            MOV [byte ptr offset filename], '1'
        PGSC1:
        cmp [ComputerWins], 2
        jne PGSC2
            MOV [byte ptr offset filename], '2'
        PGSC2:

        cmp [PlayerWins], 0
        jne PGSP0
            MOV [byte ptr offset filename+1], '0'
        PGSP0:
        cmp [PlayerWins], 1
        jne PGSP1
            MOV [byte ptr offset filename+1], '1'
        PGSP1:
        cmp [PlayerWins], 2
        jne PGSP2
            MOV [byte ptr offset filename+1], '2'
        PGSP2:


        PUSH offset filename
        MOV [imageHeight], 24
        MOV [imageLength], 28
        PUSH [imageLength]
        PUSH [imageHeight]
        MOV [ppx], 0
        MOV [ppy], 73
        CALL PrintImage
        
        ret
PrintGameStatus endp

Sleep proc
push ax
push bx
push si
push es
    SleepStart:
    cmp [sleepTime], 0
    je SleepEnd

        MOV  AX, 0040h       ; Point ES to BIOS Data Area
        MOV  ES, AX
        MOV  SI, 006Ch       ; BIOS tick count location
        MOV  BX, ES:[SI]     ; Get current tick count
        ADD  BX, 9          ; Add 18 ticks ≈ 1 second

        WAIT_:
            MOV  AX, ES:[SI] ; Load current tick count into AX
            CMP  AX, BX
            JL   WAIT_        ; Wait until 1 second has passed

    dec [sleepTime]
    jmp SleepStart

    SleepEnd:
    pop es
    pop si
    pop bx
    pop ax
    ret
Sleep endp

END MAIN
