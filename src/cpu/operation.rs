use super::CPU;

use bit_field::BitField;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Operation {
    ADC, //	....	add with carry
    AND, //	....	and (with accumulator)
    ASL, //	....	arithmetic shift left
    BCC, //	....	branch on carry clear
    BCS, //	....	branch on carry set
    BEQ, //	....	branch on equal (zero set)
    BIT, //	....	bit test
    BMI, //	....	branch on minus (negative set)
    BNE, //	....	branch on not equal (zero clear)
    BPL, //	....	branch on plus (negative clear)
    BRK, //	....	break / interrupt
    BVC, //	....	branch on overflow clear
    BVS, //	....	branch on overflow set
    CLC, //	....	clear carry
    CLD, //	....	clear decimal
    CLI, //	....	clear interrupt disable
    CLV, //	....	clear overflow
    CMP, //	....	compare (with accumulator)
    CPX, //	....	compare with X
    CPY, //	....	compare with Y
    DEC, //	....	decrement
    DEX, //	....	decrement X
    DEY, //	....	decrement Y
    EOR, //	....	exclusive or (with accumulator)
    INC, //	....	increment
    INX, //	....	increment X
    INY, //	....	increment Y
    JMP, //	....	jump
    JSR, //	....	jump subroutine
    LDA, //	....	load accumulator
    LDX, //	....	load X
    LDY, //	....	load Y
    LSR, //	....	logical shift right
    NOP, //	....	no operation
    ORA, //	....	or with accumulator
    PHA, //	....	push accumulator
    PHP, //	....	push processor status (SR)
    PLA, //	....	pull accumulator
    PLP, //	....	pull processor status (SR)
    ROL, //	....	rotate left
    ROR, //	....	rotate right
    RTI, //	....	return from interrupt
    RTS, //	....	return from subroutine
    SBC, //	....	subtract with carry
    SEC, //	....	set carry
    SED, //	....	set decimal
    SEI, //	....	set interrupt disable
    STA, //	....	store accumulator
    STX, //	....	store X
    STY, //	....	store Y
    TAX, //	....	transfer accumulator to X
    TAY, //	....	transfer accumulator to Y
    TSX, //	....	transfer stack pointer to X
    TXA, //	....	transfer X to accumulator
    TXS, //	....	transfer X to stack pointer
    TYA, //	....	transfer Y to accumulator
    XXX, // ....    invalid code
}

impl Operation {
    pub fn method(&self) -> fn(&mut CPU) {
        match self {
            Operation::ADC => CPU::adc,
            Operation::AND => CPU::and,
            Operation::ASL => CPU::asl,
            Operation::BCC => CPU::bcc,
            Operation::BCS => CPU::bcs,
            Operation::BEQ => CPU::beq,
            Operation::BIT => CPU::bit,
            Operation::BMI => CPU::bmi,
            Operation::BNE => CPU::bne,
            Operation::BPL => CPU::bpl,
            Operation::BRK => CPU::brk,
            Operation::BVC => CPU::bvc,
            Operation::BVS => CPU::bvs,
            Operation::CLC => CPU::clc,
            Operation::CLD => CPU::cld,
            Operation::CLI => CPU::cli,
            Operation::CLV => CPU::clv,
            Operation::CMP => CPU::cmp,
            Operation::CPX => CPU::cpx,
            Operation::CPY => CPU::cpy,
            Operation::DEC => CPU::dec,
            Operation::DEX => CPU::dex,
            Operation::DEY => CPU::dey,
            Operation::EOR => CPU::eor,
            Operation::INC => CPU::inc,
            Operation::INX => CPU::inx,
            Operation::INY => CPU::iny,
            Operation::JMP => CPU::jmp,
            Operation::JSR => CPU::jsr,
            Operation::LDA => CPU::lda,
            Operation::LDX => CPU::ldx,
            Operation::LDY => CPU::ldy,
            Operation::LSR => CPU::lsr,
            Operation::NOP => CPU::nop,
            Operation::ORA => CPU::ora,
            Operation::PHA => CPU::pha,
            Operation::PHP => CPU::php,
            Operation::PLA => CPU::pla,
            Operation::PLP => CPU::plp,
            Operation::ROL => CPU::rol,
            Operation::ROR => CPU::ror,
            Operation::RTI => CPU::rti,
            Operation::RTS => CPU::rts,
            Operation::SBC => CPU::sbc,
            Operation::SEC => CPU::sec,
            Operation::SED => CPU::sed,
            Operation::SEI => CPU::sei,
            Operation::STA => CPU::sta,
            Operation::STX => CPU::stx,
            Operation::STY => CPU::sty,
            Operation::TAX => CPU::tax,
            Operation::TAY => CPU::tay,
            Operation::TSX => CPU::tsx,
            Operation::TXA => CPU::txa,
            Operation::TXS => CPU::txs,
            Operation::TYA => CPU::tya,
            Operation::XXX => CPU::xxx,
        }
    }
}

impl CPU {
    //	add with carry
    fn adc(&mut self) {
        let oper = self.read_oper();

        let a1 = self.a.overflowing_add(oper);
        let a2 = a1.0.overflowing_add(self.get_flag(Self::C) as u8);

        let v = (self.a ^ oper & !(self.a ^ a2.0) & 0x80) != 0;

        self.set_flag(Self::V, v);
        self.set_flag(Self::C, a1.1 | a2.1);
        self.set_flag(Self::Z, a2.0 == 0);
        self.set_flag(Self::N, a2.0.get_bit(7));

        self.a = a2.0;
    }

    //	subtract with carry
    fn sbc(&mut self) {
        let oper = !self.read_oper();

        let a1 = self.a.overflowing_add(oper);
        let a2 = a1.0.overflowing_add(self.get_flag(Self::C) as u8);

        let v = (self.a ^ oper & !(self.a ^ a2.0) & 0x80) != 0;

        self.set_flag(Self::V, v);
        self.set_flag(Self::C, a1.1 | a2.1);
        self.set_flag(Self::Z, a2.0 == 0);
        self.set_flag(Self::N, a2.0.get_bit(7));

        self.a = a2.0;
    }
    //	and (with accumulator)
    fn and(&mut self) {
        self.a &= self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	arithmetic shift left
    fn asl(&mut self) {
        let mut v = self.read_oper();
        self.set_flag(Self::C, v.get_bit(7));
        v <<= 1;
        self.set_flag(Self::Z, v != 0);
        self.set_flag(Self::N, v.get_bit(7));

        self.write_oper(v);
    }
    //	branch on carry clear
    fn bcc(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::C), self.oper);
    }
    //	branch on carry set
    fn bcs(&mut self) {
        self.branch_on_condition(self.get_flag(Self::C), self.oper);
    }
    //	branch on equal (zero set)
    fn beq(&mut self) {
        self.branch_on_condition(self.get_flag(Self::Z), self.oper);
    }
    //	branch on not equal (zero clear)
    fn bne(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::Z), self.oper);
    }
    //	bit test
    fn bit(&mut self) {
        let v = self.read_oper();
        self.set_flag(Self::V, v.get_bit(6));
        self.set_flag(Self::N, v.get_bit(7));
        self.set_flag(Self::Z, self.a & v == 0);
    }
    //	branch on minus (negative set)
    fn bmi(&mut self) {
        self.branch_on_condition(self.get_flag(Self::N), self.oper);
    }
    //	branch on plus (negative clear)
    fn bpl(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::N), self.oper);
    }
    //  break / interrupt
    fn brk(&mut self) {
        // Set break flag
        self.set_flag(Self::U, true);
        self.set_flag(Self::B, true);

        self.push_state();

        self.set_flag(Self::I, true);

        self.pc = self.bus.borrow_mut().cpu_read(0xfffe);
    }
    //	branch on overflow clear
    fn bvc(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::V), self.oper);
    }
    //	branch on overflow set
    fn bvs(&mut self) {
        self.branch_on_condition(self.get_flag(Self::V), self.oper);
    }
    //	clear carry
    fn clc(&mut self) {
        self.set_flag(Self::C, false);
    }
    //	clear decimal
    fn cld(&mut self) {
        self.set_flag(Self::D, false);
    }
    //	clear interrupt disable
    fn cli(&mut self) {
        self.set_flag(Self::I, false);
    }
    //	clear overflow
    fn clv(&mut self) {
        self.set_flag(Self::V, false);
    }
    //	compare (with accumulator)
    fn cmp(&mut self) {
        self.compare(self.a, self.read_oper());
    }
    //	compare with X
    fn cpx(&mut self) {
        self.compare(self.x, self.read_oper());
    }
    //	compare with Y
    fn cpy(&mut self) {
        self.compare(self.y, self.read_oper());
    }
    //	decrement
    fn dec(&mut self) {
        let m = self.read_oper().wrapping_sub(1);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
        self.write_oper(m);
    }
    //	decrement X
    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	decrement Y
    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	exclusive or (with accumulator)
    fn eor(&mut self) {
        self.a ^= self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	increment
    fn inc(&mut self) {
        let m = self.bus.borrow().cpu_read::<u8>(self.oper).wrapping_add(1);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
        self.write_oper(m);
    }
    //	increment X
    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	increment Y
    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	jump
    fn jmp(&mut self) {
        self.pc = self.oper;
    }
    //	jump subroutine
    fn jsr(&mut self) {
        self.bus.borrow_mut().cpu_write(self.stack_addr(), self.pc);
        self.sp = self.sp.wrapping_sub(2);

        self.pc = self.oper;
    }
    //	return from subroutine
    fn rts(&mut self) {
        self.sp = self.sp.wrapping_add(2);
        self.pc = self.bus.borrow().cpu_read::<u16>(self.stack_addr());
    }
    //	load accumulator
    fn lda(&mut self) {
        self.a = self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	load X
    fn ldx(&mut self) {
        self.x = self.read_oper();
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	load Y
    fn ldy(&mut self) {
        self.y = self.read_oper();
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	logical shift right
    fn lsr(&mut self) {
        let mut v = self.read_oper();

        self.set_flag(Self::C, v.get_bit(0));
        v >>= 1;
        self.set_flag(Self::Z, v != 0);
        self.set_flag(Self::N, v.get_bit(7));

        self.write_oper(v);
    }
    //	no operation
    fn nop(&mut self) {}
    //	or with accumulator
    fn ora(&mut self) {
        self.a |= self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	push accumulator
    fn pha(&mut self) {
        self.bus.borrow_mut().cpu_write(self.stack_addr(), self.a);
        self.sp = self.sp.wrapping_sub(1);
    }
    //	pull accumulator
    fn pla(&mut self) {
        self.sp = self.sp.wrapping_add(1);
        self.a = self.bus.borrow().cpu_read(self.stack_addr());
        self.set_flag(Self::Z, self.a != 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	push processor status (SR)
    fn php(&mut self) {
        self.bus.borrow_mut().cpu_write(self.stack_addr(), self.st);
        self.sp = self.sp.wrapping_sub(1);
    }
    //	pull processor status (SR)
    fn plp(&mut self) {
        self.sp = self.sp.wrapping_add(1);
        self.st = self.bus.borrow().cpu_read(self.stack_addr());
    }
    //	rotate left
    fn rol(&mut self) {
        let old = self.read_oper();
        let mut new = old << 1;
        new |= self.st & 0x1;

        self.set_flag(Self::C, old.get_bit(0));
        self.set_flag(Self::Z, new == 0);
        self.set_flag(Self::N, new.get_bit(7));

        self.write_oper(new);
    }
    //	rotate right
    fn ror(&mut self) {
        let old = self.read_oper();
        let mut new = old >> 1;
        new |= self.st & 0x80;

        self.set_flag(Self::C, old.get_bit(0));
        self.set_flag(Self::Z, new == 0);
        self.set_flag(Self::N, new.get_bit(7));

        self.write_oper(new);
    }
    //	return from interrupt
    fn rti(&mut self) {
        self.pop_state();

        self.set_flag(Self::B, false);
    }
    //	set carry
    fn sec(&mut self) {
        self.set_flag(Self::C, true);
    }
    //	set decimal
    fn sed(&mut self) {
        self.set_flag(Self::D, true);
    }
    //	set interrupt disable
    fn sei(&mut self) {
        self.set_flag(Self::I, true);
    }
    //	store accumulator
    fn sta(&mut self) {
        self.bus.borrow_mut().cpu_write(self.oper, self.a);
    }
    //	store X
    fn stx(&mut self) {
        self.bus.borrow_mut().cpu_write(self.oper, self.x);
    }
    //	store Y
    fn sty(&mut self) {
        self.bus.borrow_mut().cpu_write(self.oper, self.y);
    }
    //	transfer accumulator to X
    fn tax(&mut self) {
        self.x = self.a;
    }
    //	transfer accumulator to Y
    fn tay(&mut self) {
        self.y = self.a
    }
    //	transfer stack pointer to X
    fn tsx(&mut self) {
        self.x = self.sp;
    }
    //	transfer X to accumulator
    fn txa(&mut self) {
        self.a = self.x;
    }
    //	transfer X to stack pointer
    fn txs(&mut self) {
        self.sp = self.x;
    }
    //	transfer Y to accumulator
    fn tya(&mut self) {
        self.a = self.y;
    }
}
