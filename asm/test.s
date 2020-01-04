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

    sed
    sec
    brk
    jmp forever

irq:
    clc
    cld
    rti

forever:
    jmp forever