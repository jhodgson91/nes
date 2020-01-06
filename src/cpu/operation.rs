use super::{Bus, Flags, CPU};

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
    pub fn method(&self) -> fn(&mut CPU, &mut Bus) {
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
    fn adc(&mut self, bus: &mut Bus) {
        let oper = self.read_oper(bus);

        let a1 = self.a.overflowing_add(oper);
        let a2 = a1.0.overflowing_add(self.get_flag(Flags::C) as u8);

        let v = (self.a ^ oper & !(self.a ^ a2.0) & 1 << 7) != 0;

        self.set_flag(Flags::V, v);
        self.set_flag(Flags::C, a1.1 | a2.1);
        self.set_flag(Flags::Z, a2.0 == 0);
        self.set_flag(Flags::N, a2.0 & 1 << 7 != 0);

        self.a = a2.0;
    }

    //	subtract with carry
    fn sbc(&mut self, bus: &mut Bus) {
        let oper = !self.read_oper(bus);

        let a1 = self.a.overflowing_add(oper);
        let a2 = a1.0.overflowing_add(self.get_flag(Flags::C) as u8);

        let v = (self.a ^ oper & !(self.a ^ a2.0) & 1 << 7) != 0;

        self.set_flag(Flags::V, v);
        self.set_flag(Flags::C, a1.1 | a2.1);
        self.set_flag(Flags::Z, a2.0 == 0);
        self.set_flag(Flags::N, a2.0 & 1 << 7 != 0);

        self.a = a2.0;
    }
    //	and (with accumulator)
    fn and(&mut self, bus: &mut Bus) {
        self.a &= self.read_oper(bus);
        self.set_flag(Flags::Z, self.a == 0);
        self.set_flag(Flags::N, self.a & 1 << 7 != 0);
    }
    //	arithmetic shift left
    fn asl(&mut self, bus: &mut Bus) {
        let mut v = self.read_oper(bus);
        self.set_flag(Flags::C, v & 1 << 7 != 0);
        v <<= 1;
        self.set_flag(Flags::Z, v != 0);
        self.set_flag(Flags::N, v & 1 << 7 != 0);

        self.write_oper(bus, v);
    }
    //	branch on carry clear
    fn bcc(&mut self, _: &mut Bus) {
        self.branch_on_condition(!self.get_flag(Flags::C), self.oper);
    }
    //	branch on carry set
    fn bcs(&mut self, _: &mut Bus) {
        self.branch_on_condition(self.get_flag(Flags::C), self.oper);
    }
    //	branch on equal (zero set)
    fn beq(&mut self, _: &mut Bus) {
        self.branch_on_condition(self.get_flag(Flags::Z), self.oper);
    }
    //	branch on not equal (zero clear)
    fn bne(&mut self, _: &mut Bus) {
        self.branch_on_condition(!self.get_flag(Flags::Z), self.oper);
    }
    //	bit test
    fn bit(&mut self, bus: &mut Bus) {
        let v = self.read_oper(bus);
        self.set_flag(Flags::V, v & 1 << 6 != 0);
        self.set_flag(Flags::N, v & 1 << 7 != 0);
        self.set_flag(Flags::Z, self.a & v == 0);
    }
    //	branch on minus (negative set)
    fn bmi(&mut self, _: &mut Bus) {
        self.branch_on_condition(self.get_flag(Flags::N), self.oper);
    }
    //	branch on plus (negative clear)
    fn bpl(&mut self, _: &mut Bus) {
        self.branch_on_condition(!self.get_flag(Flags::N), self.oper);
    }
    //  break / interrupt
    fn brk(&mut self, bus: &mut Bus) {
        // Set break flag
        self.set_flag(Flags::U, true);
        self.set_flag(Flags::B, true);

        self.push_state(bus);

        self.set_flag(Flags::I, true);

        self.pc = bus.cpu_read(0xfffe);
    }
    //	branch on overflow clear
    fn bvc(&mut self, _: &mut Bus) {
        self.branch_on_condition(!self.get_flag(Flags::V), self.oper);
    }
    //	branch on overflow set
    fn bvs(&mut self, _: &mut Bus) {
        self.branch_on_condition(self.get_flag(Flags::V), self.oper);
    }
    //	clear carry
    fn clc(&mut self, _: &mut Bus) {
        self.set_flag(Flags::C, false);
    }
    //	clear decimal
    fn cld(&mut self, _: &mut Bus) {
        self.set_flag(Flags::D, false);
    }
    //	clear interrupt disable
    fn cli(&mut self, _: &mut Bus) {
        self.set_flag(Flags::I, false);
    }
    //	clear overflow
    fn clv(&mut self, _: &mut Bus) {
        self.set_flag(Flags::V, false);
    }
    //	compare (with accumulator)
    fn cmp(&mut self, bus: &mut Bus) {
        self.compare(self.a, self.read_oper(bus));
    }
    //	compare with X
    fn cpx(&mut self, bus: &mut Bus) {
        self.compare(self.x, self.read_oper(bus));
    }
    //	compare with Y
    fn cpy(&mut self, bus: &mut Bus) {
        self.compare(self.y, self.read_oper(bus));
    }
    //	decrement
    fn dec(&mut self, bus: &mut Bus) {
        let m = self.read_oper(bus).wrapping_sub(1);
        self.set_flag(Flags::Z, m == 0);
        self.set_flag(Flags::N, m & 1 << 7 != 0);
        self.write_oper(bus, m);
    }
    //	decrement X
    fn dex(&mut self, _: &mut Bus) {
        self.x = self.x.wrapping_sub(1);
        self.set_flag(Flags::Z, self.x == 0);
        self.set_flag(Flags::N, self.x & 1 << 7 != 0);
    }
    //	decrement Y
    fn dey(&mut self, _: &mut Bus) {
        self.y = self.y.wrapping_sub(1);
        self.set_flag(Flags::Z, self.y == 0);
        self.set_flag(Flags::N, self.y & 1 << 7 != 0);
    }
    //	exclusive or (with accumulator)
    fn eor(&mut self, bus: &mut Bus) {
        self.a ^= self.read_oper(bus);
        self.set_flag(Flags::Z, self.a == 0);
        self.set_flag(Flags::N, self.a & 1 << 7 != 0);
    }
    //	increment
    fn inc(&mut self, bus: &mut Bus) {
        let m = bus.cpu_read::<u8>(self.oper).wrapping_add(1);
        self.set_flag(Flags::Z, m == 0);
        self.set_flag(Flags::N, m & 1 << 7 != 0);
        self.write_oper(bus, m);
    }
    //	increment X
    fn inx(&mut self, _: &mut Bus) {
        self.x = self.x.wrapping_add(1);
        self.set_flag(Flags::Z, self.x == 0);
        self.set_flag(Flags::N, self.x & 1 << 7 != 0);
    }
    //	increment Y
    fn iny(&mut self, _: &mut Bus) {
        self.y = self.y.wrapping_add(1);
        self.set_flag(Flags::Z, self.y == 0);
        self.set_flag(Flags::N, self.y & 1 << 7 != 0);
    }
    //	jump
    fn jmp(&mut self, _: &mut Bus) {
        self.pc = self.oper;
    }
    //	jump subroutine
    fn jsr(&mut self, bus: &mut Bus) {
        self.sp = self.sp.wrapping_sub(1);
        bus.cpu_write(self.stack_addr(), self.pc);
        self.sp = self.sp.wrapping_sub(1);

        self.pc = self.oper;
    }
    //	return from subroutine
    fn rts(&mut self, bus: &mut Bus) {
        self.sp = self.sp.wrapping_add(1);
        self.pc = bus.cpu_read::<u16>(self.stack_addr());
        self.sp = self.sp.wrapping_add(1);
    }
    //	load accumulator
    fn lda(&mut self, bus: &mut Bus) {
        self.a = self.read_oper(bus);
        self.set_flag(Flags::Z, self.a == 0);
        self.set_flag(Flags::N, self.a & 1 << 7 != 0);
    }
    //	load X
    fn ldx(&mut self, bus: &mut Bus) {
        self.x = self.read_oper(bus);
        self.set_flag(Flags::Z, self.x == 0);
        self.set_flag(Flags::N, self.x & 1 << 7 != 0);
    }
    //	load Y
    fn ldy(&mut self, bus: &mut Bus) {
        self.y = self.read_oper(bus);
        self.set_flag(Flags::Z, self.y == 0);
        self.set_flag(Flags::N, self.y & 1 << 7 != 0);
    }
    //	logical shift right
    fn lsr(&mut self, bus: &mut Bus) {
        let mut v = self.read_oper(bus);

        self.set_flag(Flags::C, v & 1 << 0 != 0);
        v >>= 1;
        self.set_flag(Flags::Z, v != 0);
        self.set_flag(Flags::N, v & 1 << 7 != 0);

        self.write_oper(bus, v);
    }
    //	no operation
    fn nop(&mut self, _: &mut Bus) {}
    //	or with accumulator
    fn ora(&mut self, bus: &mut Bus) {
        self.a |= self.read_oper(bus);
        self.set_flag(Flags::Z, self.a == 0);
        self.set_flag(Flags::N, self.a & 1 << 7 != 0);
    }
    //	push accumulator
    fn pha(&mut self, bus: &mut Bus) {
        bus.cpu_write(self.stack_addr(), self.a);
        self.sp = self.sp.wrapping_sub(1);
    }
    //	pull accumulator
    fn pla(&mut self, bus: &mut Bus) {
        self.sp = self.sp.wrapping_add(1);
        self.a = bus.cpu_read(self.stack_addr());
        self.set_flag(Flags::Z, self.a != 0);
        self.set_flag(Flags::N, self.a & 1 << 7 != 0);
    }
    //	push processor status (SR)
    fn php(&mut self, bus: &mut Bus) {
        bus.cpu_write(self.stack_addr(), self.st.bits());
        self.sp = self.sp.wrapping_sub(1);
    }
    //	pull processor status (SR)
    fn plp(&mut self, bus: &mut Bus) {
        self.sp = self.sp.wrapping_add(1);
        self.st = Flags::from_bits(bus.cpu_read(self.stack_addr())).unwrap();
    }
    //	rotate left
    fn rol(&mut self, bus: &mut Bus) {
        let old = self.read_oper(bus);
        let mut new = old << 1;
        new |= self.st.bits() & 0x1;

        self.set_flag(Flags::C, old & 0x1 != 0);
        self.set_flag(Flags::Z, new == 0);
        self.set_flag(Flags::N, new & 1 << 7 != 0);

        self.write_oper(bus, new);
    }
    //	rotate right
    fn ror(&mut self, bus: &mut Bus) {
        let old = self.read_oper(bus);
        let mut new = old >> 1;
        new |= self.st.bits() & 1 << 7;

        self.set_flag(Flags::C, old & 0x1 != 0);
        self.set_flag(Flags::Z, new == 0);
        self.set_flag(Flags::N, new & 1 << 7 != 0);

        self.write_oper(bus, new);
    }
    //	return from interrupt
    fn rti(&mut self, bus: &mut Bus) {
        self.pop_state(bus);

        self.set_flag(Flags::B, false);
    }
    //	set carry
    fn sec(&mut self, _: &mut Bus) {
        self.set_flag(Flags::C, true);
    }
    //	set decimal
    fn sed(&mut self, _: &mut Bus) {
        self.set_flag(Flags::D, true);
    }
    //	set interrupt disable
    fn sei(&mut self, _: &mut Bus) {
        self.set_flag(Flags::I, true);
    }
    //	store accumulator
    fn sta(&mut self, bus: &mut Bus) {
        bus.cpu_write(self.oper, self.a);
    }
    //	store X
    fn stx(&mut self, bus: &mut Bus) {
        bus.cpu_write(self.oper, self.x);
    }
    //	store Y
    fn sty(&mut self, bus: &mut Bus) {
        bus.cpu_write(self.oper, self.y);
    }
    //	transfer accumulator to X
    fn tax(&mut self, _: &mut Bus) {
        self.x = self.a;
    }
    //	transfer accumulator to Y
    fn tay(&mut self, _: &mut Bus) {
        self.y = self.a
    }
    //	transfer stack pointer to X
    fn tsx(&mut self, _: &mut Bus) {
        self.x = self.sp;
    }
    //	transfer X to accumulator
    fn txa(&mut self, _: &mut Bus) {
        self.a = self.x;
    }
    //	transfer X to stack pointer
    fn txs(&mut self, _: &mut Bus) {
        self.sp = self.x;
    }
    //	transfer Y to accumulator
    fn tya(&mut self, _: &mut Bus) {
        self.a = self.y;
    }
}
