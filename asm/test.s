.segment "CHARS"
.segment "HEADER"
    .byte "NES",26,2,1 ; 32K PRG, 8K CHR
.segment "VECTORS"
    .word nmi, reset, irq
.segment "STARTUP" ; avoids warning
.segment "CODE"

nmi:
reset:
    cli

    ldx #$ff
    txs

    brk

    jsr set
    jsr clear
    jmp forever

irq:
    rti

clear:
    clc
    cld
    rts

set:
    sec
    sed
    rts

forever:
    jmp forever