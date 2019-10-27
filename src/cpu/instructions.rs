use super::CPU;

#[derive(Copy, Clone)]
pub struct Instruction {
    pub name: &'static str,
    pub op: &'static dyn Fn(&mut CPU, u16) -> u8,
    pub addr_mode: &'static dyn Fn(&mut CPU) -> (u16, u8),
    pub cycles: u8,
}

impl Instruction {
    pub const INVALID: Instruction = Instruction {
        name: "???",
        op: &|_, _| unimplemented!(),
        addr_mode: &|_| unimplemented!(),
        cycles: 0,
    };
}

impl CPU {
    //	add with carry
    pub fn adc(&mut self, oper: u16) -> u8 {
        0
    }
    //	and (with accumulator)
    pub fn and(&mut self, oper: u16) -> u8 {
        0
    }
    //	arithmetic shift left
    pub fn asl(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on carry clear
    pub fn bcc(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on carry set
    pub fn bcs(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on equal (zero set)
    pub fn beq(&mut self, oper: u16) -> u8 {
        0
    }
    //	bit test
    pub fn bit(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on minus (negative set)
    pub fn bmi(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on not equal (zero clear)
    pub fn bne(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on plus (negative clear)
    pub fn bpl(&mut self, oper: u16) -> u8 {
        0
    }
    //  break / interrupt
    pub fn brk(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on overflow clear
    pub fn bvc(&mut self, oper: u16) -> u8 {
        0
    }
    //	branch on overflow set
    pub fn bvs(&mut self, oper: u16) -> u8 {
        0
    }
    //	clear carry
    pub fn clc(&mut self, oper: u16) -> u8 {
        0
    }
    //	clear decimal
    pub fn cld(&mut self, oper: u16) -> u8 {
        0
    }
    //	clear interrupt disable
    pub fn cli(&mut self, oper: u16) -> u8 {
        0
    }
    //	clear overflow
    pub fn clv(&mut self, oper: u16) -> u8 {
        0
    }
    //	compare (with accumulator)
    pub fn cmp(&mut self, oper: u16) -> u8 {
        0
    }
    //	compare with X
    pub fn cpx(&mut self, oper: u16) -> u8 {
        0
    }
    //	compare with Y
    pub fn cpy(&mut self, oper: u16) -> u8 {
        0
    }
    //	decrement
    pub fn dec(&mut self, oper: u16) -> u8 {
        0
    }
    //	decrement X
    pub fn dex(&mut self, oper: u16) -> u8 {
        0
    }
    //	decrement Y
    pub fn dey(&mut self, oper: u16) -> u8 {
        0
    }
    //	exclusive or (with accumulator)
    pub fn eor(&mut self, oper: u16) -> u8 {
        0
    }
    //	increment
    pub fn inc(&mut self, oper: u16) -> u8 {
        0
    }
    //	increment X
    pub fn inx(&mut self, oper: u16) -> u8 {
        0
    }
    //	increment Y
    pub fn iny(&mut self, oper: u16) -> u8 {
        0
    }
    //	jump
    pub fn jmp(&mut self, oper: u16) -> u8 {
        0
    }
    //	jump subroutine
    pub fn jsr(&mut self, oper: u16) -> u8 {
        0
    }
    //	load accumulator
    pub fn lda(&mut self, oper: u16) -> u8 {
        0
    }
    //	load X
    pub fn ldx(&mut self, oper: u16) -> u8 {
        0
    }
    //	load Y
    pub fn ldy(&mut self, oper: u16) -> u8 {
        0
    }
    //	logical shift right
    pub fn lsr(&mut self, oper: u16) -> u8 {
        0
    }
    //	no operation
    pub fn nop(&mut self, oper: u16) -> u8 {
        0
    }
    //	or with accumulator
    pub fn ora(&mut self, oper: u16) -> u8 {
        0
    }
    //	push accumulator
    pub fn pha(&mut self, oper: u16) -> u8 {
        0
    }
    //	push processor status (SR)
    pub fn php(&mut self, oper: u16) -> u8 {
        0
    }
    //	pull accumulator
    pub fn pla(&mut self, oper: u16) -> u8 {
        0
    }
    //	pull processor status (SR)
    pub fn plp(&mut self, oper: u16) -> u8 {
        0
    }
    //	rotate left
    pub fn rol(&mut self, oper: u16) -> u8 {
        0
    }
    //	rotate right
    pub fn ror(&mut self, oper: u16) -> u8 {
        0
    }
    //	return from interrupt
    pub fn rti(&mut self, oper: u16) -> u8 {
        0
    }
    //	return from subroutine
    pub fn rts(&mut self, oper: u16) -> u8 {
        0
    }
    //	subtract with carry
    pub fn sbc(&mut self, oper: u16) -> u8 {
        0
    }
    //	set carry
    pub fn sec(&mut self, oper: u16) -> u8 {
        0
    }
    //	set decimal
    pub fn sed(&mut self, oper: u16) -> u8 {
        0
    }
    //	set interrupt disable
    pub fn sei(&mut self, oper: u16) -> u8 {
        0
    }
    //	store accumulator
    pub fn sta(&mut self, oper: u16) -> u8 {
        0
    }
    //	store X
    pub fn stx(&mut self, oper: u16) -> u8 {
        0
    }
    //	store Y
    pub fn sty(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer accumulator to X
    pub fn tax(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer accumulator to Y
    pub fn tay(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer stack pointer to X
    pub fn tsx(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer X to accumulator
    pub fn txa(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer X to stack pointer
    pub fn txs(&mut self, oper: u16) -> u8 {
        0
    }
    //	transfer Y to accumulator
    pub fn tya(&mut self, oper: u16) -> u8 {
        0
    }
}
