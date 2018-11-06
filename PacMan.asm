.segment "CODE"
;================================adresses===================================
 BACKGROUND_COLOR = $5100    ; r/w the background color
 BACKGROUND_SCROLL_X = $5101 ; r/w the x position of the tiles
 BACKGROUND_SCROLL_Y = $5102 ; r/w the y position of the tiles

 CHR_COL = $5103             ; r/w the current column of the character rom
 CHR_ROW = $5104             ; r/w the current row of the character rom
 CHR_PIXEL = $5105           ; r/w the pixel at the current row and column of
                             ; the character rom

 INPUT = $5106               ; r the current pressed keys

 PLAYERSPRITE = $5000        ; every sprite is located at page $50 the other
                             ; bits are used for the scale, flipX, flipY
                             ; the displayed tile and if it's enabled in this
                             ; order: 0=tile, 1=x, 2=y, 3=flags: 
                             ; 3a=disable/enable, 3b/3c=flipX/Y, 3d=8x8 or 
                             ; 16x16 3eTo3h=scale
 REDGHOSTSPRITE = $5004      ; red ghost sprite 

 BACKGROUND_MODE = $5107     ; bits: 0=8x8 or 16x16mode, 1/2=select background

 BACKGROUND0 = $4000         ; nametable starting at 0,0
 BACKGROUND1 = $4400         ; nametable starting at 0,1
 BACKGROUND2 = $4800         ; nametable starting at 1,0
 BACKGROUND3 = $4c00         ; nametable starting at 1,1
 ;================================adresses===================================
 
;=================================macros====================================
 .macro m_setTileWithXOffset background,mapNumber
    LDA mapNumber,X
    STA background,X
 .endmacro
 ;=================================macros====================================

;==================================code=====================================
 LDA #$01               
 STA BACKGROUND_MODE    ; set tile background mode to be 16x16
 LDA #$03
 STA BACKGROUND_COLOR   ; set the background color behind the tiles

 LDX #0
 fill0:
    m_setTileWithXOffset BACKGROUND0,map0   ; fill the first background map
    INX
    BNE fill0

 LDA #$B4                                   
 STA PLAYERSPRITE                           ; set the player sprite
 LDA #$D0
 STA PLAYERSPRITE+1                         ; set the player x
 STA PLAYERSPRITE+2                         ; set the player y
 LDA #$09   
 STA PLAYERSPRITE+3                         ; set the player flags

 LDA #$B3
 STA REDGHOSTSPRITE
 LDA #$20
 STA REDGHOSTSPRITE+1
 STA REDGHOSTSPRITE+2
 LDA #$09
 STA REDGHOSTSPRITE+3
 STA redghostDirection

 gameLoop:
    JMP gameLoop

 setRedGhostDirection:
     CLC
     LDA REDGHOSTSPRITE+1; load ghost X
     ADC #$08            ; center ghost X
     LSR A               ; divide by 2 to the power of 4
     LSR A
     LSR A
     LSR A
     STA redghostX        ; set the current X tile where the ghost is currently at
     CLC                 ; clear the carry that could have occurred by shifting

     LDA REDGHOSTSPRITE+2; load ghost y
     ADC #$08            ; center ghost Y
     LSR A               ; divide by 2 to the power of 4
     LSR A
     LSR A
     LSR A
     STA redghostY      ; set the current Y tile the ghost is currently at
     CLC                 ; clear the carry that could have occurred by shifting

     ASL A               ; multiply by 2 to the power of 4
     ASL A
     ASL A
     ASL A
     CLC                 ; clear the carry that could have occurred by shifting
     ADC redghostX       ; after doing Y * 16 i Add X to that to have the right
                         ; index for the background tilemap

     STA redghostIndex   ; transfer the value of the accomulator to zeropage+4
     LDA #$FF
     STA redghostBestDistance ; zeropage+1 will store the best distance from player, 
                         ; setting it at #$FF means that any other distance will
                         ; be closer than that
     CLC    
     rg_getUpTile:
     LDA redghostDirection
     CMP #02             ; 02 = down
     BEQ rg_getDownTile  ; if going downn i can't go up
     LDA redghostIndex   ; load the ghost current index
     SEC
     SBC #$10            ; subtract 16 to get one row lower
     TAX
     LDA BACKGROUND0, X
     BEQ rg_getDownTile
     TXA
     SEC
     SBC playerIndex     ; subtract the current player index with the current 
                         ; ghost index
     BCS upPositive
     EOR #$FF
     ADC #$01
     upPositive:
     STA redghostBestDistance     ; save at zeroPage+1 the distance just found
     LDA #$01                       ; 01 = up
     STA redghostBestDirection      ; save current best direction at zeropage+2

     rg_getDownTile:
     LDA redghostDirection
     CMP #01             ; 01 = up
     BEQ rg_getRightTile ; if going up i can't go down
     CLC
     LDA redghostIndex   ; load the ghost current index
     ADC #$10            ; add 16 to get one row higher
     TAX
     LDA BACKGROUND0, X
     BEQ rg_getRightTile
     TXA
     SEC                 ; not sure about this.
     SBC playerIndex     ; subtract the current player index with the current 
                         ; ghost index
     BCS downPositive
     EOR #$FF
     ADC #$01
     downPositive:
     CMP redghostBestDistance      ; compare the distance we found with the latest stored
     BCS rg_getRightTile ; if the distance found before is smaller than the one 
                         ; we just found, we try the next
     STA redghostBestDistance      ; save at zeroPage+1 the distance just found because it
                         ; means it's better than the other one
     LDA #$02            ; 02 = down            
     STA redghostBestDirection      ; save current best direction at zeropage+2

     rg_getRightTile:
     LDA redghostDirection
     CMP #08             ; 08 = left
     BEQ rg_getLeftTile  ; if going left i can't go down
     CLC
     LDA redghostIndex      ; load the ghost current index
     ADC #$01            ; add 01 to get the index 1 column higher
     TAX
     LDA BACKGROUND0, X
     BEQ rg_getLeftTile
     TXA
     SEC
     SBC playerIndex     ; subtract the current player index with the current 
                         ; ghost index
     BCS rightPositive
     EOR #$FF
     ADC #$01
     rightPositive:
     CMP redghostBestDistance      ; compare the distance we found with the latest stored
     BCS rg_getLeftTile  ; if the distance found before is smaller than the one 
                         ; we just found, we try the next
     STA redghostBestDistance      ; save at zeroPage+1 the distance just found because it
                         ; means it's better than the other one
     LDA #$04            ; 04 = right            
     STA redghostBestDirection     ; save current best direction at zeropage+2
     rg_getLeftTile:
     LDA redghostDirection
     CMP #04             ; 04 = right
     BEQ rg_end  ; if going right i can't go down
     LDA redghostIndex      ; load the ghost current index
     SEC
     SBC #$01            ; subtract 01 to get the index 1 column lower
     TAX
     LDA BACKGROUND0, X
     BEQ rg_end
     TXA
     SEC
     SBC playerIndex     ; subtract the current player index with the current 
                         ; ghost index
     BCS leftPositive
     EOR #$FF
     ADC #$01
     leftPositive:
     CMP redghostBestDistance      ; compare the distance we found with the latest stored
     BCS rg_end  ; if the distance found before is smaller than the one 
                         ; we just found, we try the next
     STA redghostBestDistance      ; save at zeroPage+1 the distance just found because it
                         ; means it's better than the other one
     LDA #$08            ; 08 = left            
     STA redghostBestDirection     ; save current best direction at zeropage+2
     rg_end:
     LDA redghostBestDirection      ; load the current best direction
     STA redghostDirection
     RTS

 moveGhost:
    LDA redghostDirection
    CMP #$01
    BNE redGhostDown
    SEC
    LDA REDGHOSTSPRITE+2
    SBC #$10   
    STA REDGHOSTSPRITE+2
    redGhostDown:
        LDA redghostDirection
        CMP #$02
        BNE redGhostRight
        CLC
        LDA REDGHOSTSPRITE+2
        ADC #$10
        STA REDGHOSTSPRITE+2
    redGhostRight:
        LDA redghostDirection
        CMP #$04
        BNE redGhostLeft
        CLC
        LDA REDGHOSTSPRITE+1
        ADC #$10
        STA REDGHOSTSPRITE+1
    redGhostLeft:
        LDA redghostDirection
        CMP #$08
        BNE endMoveGhost
        SEC
        LDA REDGHOSTSPRITE+1
        SBC #$10
        STA REDGHOSTSPRITE+1
    endMoveGhost:
        RTS

 getPlayerInput:
    checkUp:
        LDA INPUT       ; get the input
        AND #$01        ; check if up is being pressed
        BEQ checkDown   ; if not being pressed we jump to the next check
        JSR moveUp      ; if it's being pressed we move and then keep going to
                        ; the next instruction
        
    checkDown:          ; same as checkUp
        LDA INPUT
        AND #$02
        BEQ checkLeft
        JSR moveDown

    checkLeft:          ; same as checkUp
        LDA INPUT
        AND #$08
        BEQ checkRight
        JSR moveLeft

    checkRight:         ; same as checkUp
        LDA INPUT
        AND #$04
        BEQ endPlayerInput
        JSR moveRight
    endPlayerInput:
        RTS
        
 setPlayerTile:
    CLC
    LDA PLAYERSPRITE+1  ; load player X
    ADC #$08            ; center player X
    LSR A               ; divide by 2 to the power of 4
    LSR A
    LSR A
    LSR A
    STA playerCurrTileX ; set the current X tile the player is currently at
    CLC                 ; clear the carry that could have occurred by shifting

    LDA PLAYERSPRITE+2  ; load player y
    ADC #$08            ; center player Y
    LSR A               ; divide by 2 to the power of 4
    LSR A
    LSR A
    LSR A
    STA playerCurrTileY ; set the current X tile the player is currently at
    CLC                 ; clear the carry that could have occurred by shifting

    LDA playerCurrTileY ; load the Y tile value
    ASL A               ; multiply by 2 to the power of 4
    ASL A
    ASL A
    ASL A
    CLC                 ; clear the carry that could have occurred by shifting
    ADC playerCurrTileX ; after doing Y * 16 i Add X to that to have the right
                        ; index for the background tilemap

    STA playerIndex     ; transfer the value of the accomulator to X register
    RTS

 moveUp:
    LDA playerIndex     ; load the tile where the current player is
    SEC                 ; set the carry before subtraction
    SBC #$10            ; subtract by 16 the tile where the player currently is
                        ; in order to get the tile on top of him
    TAX                 ; load the value into x
    LDA BACKGROUND0, X  ; get the value of the tile stored at the x position of 
                        ; the background
    BEQ endMoveUp       ; if the value is 0 then it is going to collide so we don't
                        ; move
    SEC
    LDA PLAYERSPRITE+2
    SBC #$10            ; if it's not 0 we move
    STA PLAYERSPRITE+2

    endMoveUp:
        RTS

 moveDown:              ; same as moveUp but we add 16 to get the tile under us
    LDA playerIndex
    CLC
    ADC #$10
    TAX
    LDA BACKGROUND0, X
    BEQ endMoveDown
    CLC
    LDA PLAYERSPRITE+2
    ADC #$10
    STA PLAYERSPRITE+2

    endMoveDown:
        RTS

 moveRight:             ; same as moveUp but we add 1 to get the tile on our right
    LDA playerIndex
    CLC
    ADC #$01
    TAX
    LDA BACKGROUND0, X
    BEQ endMoveRight
    CLC
    LDA PLAYERSPRITE+1
    ADC #$10
    STA PLAYERSPRITE+1
    
    endMoveRight:
        RTS

 moveLeft:              ; same as moveUp but we sebtract 1 to get the tile on our
                        ; left
    LDA playerIndex
    CLC
    SEC
    SBC #$01
    TAX
    LDA BACKGROUND0, X
    BEQ endMoveLeft
    SEC
    LDA PLAYERSPRITE+1
    SBC #$10   
    STA PLAYERSPRITE+1

    endMoveLeft:
        RTS

 vblank:
    
    JSR setPlayerTile   ; set the player tile
    
    INC counter
    LDA counter
    CMP #$10
    BNE endVblank
    LDA #$00
    STA counter
    JSR getPlayerInput

    JSR setRedGhostDirection
    JSR moveGhost

    endVblank:
    RTI

 map0:                  ; map0 that we will use to fill BACKGROUND0 we use 
                        ; the block 000 as a collider right now
    .byte 000,000,000,224,000,000,000,000,000,000,224,000,000,000,000,000
    .byte 000,000,000,224,000,000,000,000,000,000,224,000,000,000,000,000
    .byte 000,000,224,224,224,224,224,224,224,224,224,224,224,000,000,000
    .byte 000,000,224,000,000,224,000,000,000,000,224,000,224,224,000,000
    .byte 224,224,224,000,000,224,224,224,000,000,224,000,000,224,224,224
    .byte 000,000,224,000,000,000,000,224,000,000,224,000,000,224,000,000
    .byte 224,224,224,000,000,224,224,224,224,224,224,224,224,224,224,224
    .byte 000,000,224,224,224,224,000,224,000,000,224,000,000,224,000,000
    .byte 000,000,000,000,000,224,000,224,000,224,224,224,224,224,000,000
    .byte 224,224,224,000,000,224,000,224,000,224,000,000,000,224,224,224
    .byte 000,000,224,224,224,224,000,224,000,224,000,224,224,224,000,000
    .byte 000,000,224,000,000,224,224,224,224,224,000,224,000,224,000,000
    .byte 000,000,224,224,000,224,000,000,000,224,000,224,000,224,000,000
    .byte 000,000,000,224,224,224,224,224,224,224,224,224,224,224,000,000
    .byte 000,000,000,224,000,000,000,000,000,000,224,000,000,000,000,000
    .byte 000,000,000,224,000,000,000,000,000,000,224,000,000,000,000,000


 ;===================================code====================================

.segment "RAM"
    zeroPage:           ; reserving all the zeropage bytes to keep it clean
        .RES 256
    playerCurrTileX:    ; variable to store the playerTileX
        .byte 00
    playerCurrTileY:    ; variable to store the playerTileY
        .byte 00
    playerIndex:        ; variable to store the player current index inside the 
                        ; map0 
        .byte 00
    redghostDirection:  ; #01 up, #02 down, #04 right, #08 left
        .byte 00
    redghostBestDirection:
        .byte 00
    redghostBestDistance:
        .byte 00
    redghostIndex:
        .byte 00
    redghostX:
        .byte 00
    redghostY:
        .byte 00
    counter:
        .byte 00
.segment "VECTORS"
    .word vblank