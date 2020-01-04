.segment "CHARS"
.segment "HEADER"
    .byte "NES",26,2,1 ; 32K PRG, 8K CHR
.segment "VECTORS"
    .word nmi, reset, irq
.segment "STARTUP" ; avoids warning
.segment "CODE"

nmi:
irq:
reset:
    cli
    jsr test
    jmp forever

test:
    lda #$f0
    sta $0000
    lda #$0f
    eor $0000
    rts
forever:
    jmp forever