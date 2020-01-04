.segment "CHARS"
.segment "HEADER"
    .byte "NES",26,2,1 ; 32K PRG, 8K CHR
.segment "VECTORS"
    .word 0, reset, 0
.segment "STARTUP" ; avoids warning
.segment "CODE"

reset:
    jsr test
    jmp forever

test:
    lda #0
    loop:
        adc #1
        cmp #10
        bne loop
    rts
nmi:
irq:
forever:
    jmp forever