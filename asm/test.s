.segment "CHARS"
.segment "HEADER"
    .byte "NES",26,2,1 ; 32K PRG, 8K CHR
.segment "VECTORS"
    .word nmi, reset, irq
.segment "STARTUP" ; avoids warning
.segment "CODE"

reset:
    cli
    jsr test
    jmp forever

test:
    lda #0
    clc
    count_5:
        adc #1
        cmp #5
        bne count_5
        sta $0000

    rts
nmi:
irq:
    lda #20
    clc
    count_4:
        adc #1
        cmp #24
        bne count_4
    sta $0001
    rti
forever:
    jmp forever