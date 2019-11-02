use super::address_modes::AddressMode;
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
    pub fn method(&self) -> impl Fn(&mut CPU) {
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

#[derive(Copy, Clone)]
pub struct Instruction {
    pub code: u8,
    pub operation: Operation,
    pub addr_mode: AddressMode,
    pub cycles: u8,
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            fmt,
            "[{} : {:?},{:?} - {}]",
            self.code, self.operation, self.addr_mode, self.cycles
        )
    }
}

impl Instruction {
    pub const INVALID: Instruction = Instruction {
        code: 0xff,
        operation: Operation::XXX,
        addr_mode: AddressMode::XXX,
        cycles: 0,
    };
}

impl CPU {
    const fn make_instructions() -> [Instruction; 16 * 16] {
        let mut res = [Instruction::INVALID; 16 * 16];
        macro_rules! instruction {
            ($code:literal, $ins:ident, $addr: ident, $cycles:literal) => {
                res[$code] = Instruction {
                    code: $code,
                    operation: Operation::$ins,
                    addr_mode: AddressMode::$addr,
                    cycles: $cycles,
                }
            };
        }

        // adc
        instruction!(0x69, ADC, IMM, 2);
        instruction!(0x65, ADC, ZP0, 3);
        instruction!(0x69, ADC, ZPX, 4);
        instruction!(0x6d, ADC, AB0, 4);
        instruction!(0x7d, ADC, ABX, 4);
        instruction!(0x79, ADC, ABY, 4);
        instruction!(0x61, ADC, IDX, 6);
        instruction!(0x71, ADC, IDY, 5);

        // AND - Logical AND
        instruction!(0x29, AND, IMM, 2);
        instruction!(0x25, AND, ZP0, 3);
        instruction!(0x35, AND, ZPX, 4);
        instruction!(0x2d, AND, AB0, 4);
        instruction!(0x3d, AND, ABX, 4);
        instruction!(0x39, AND, ABY, 4);
        instruction!(0x21, AND, IDX, 6);
        instruction!(0x31, AND, IDY, 5);

        // asl - arithmetic shift left
        instruction!(0x0a, ASL, ACC, 2);
        instruction!(0x06, ASL, ZP0, 5);
        instruction!(0x16, ASL, ZPX, 6);
        instruction!(0x0e, ASL, AB0, 6);
        instruction!(0x1e, ASL, ABX, 7);

        // branches
        instruction!(0x90, BCC, REL, 2);
        instruction!(0xb0, BCS, REL, 2);
        instruction!(0xf0, BEQ, REL, 2);
        instruction!(0x30, BMI, REL, 2);
        instruction!(0xd0, BNE, REL, 2);
        instruction!(0x10, BPL, REL, 2);
        instruction!(0x50, BVC, REL, 2);
        instruction!(0x70, BVS, REL, 2);

        // BRK - Force Interrupt
        instruction!(0x00, BRK, IMP, 7);

        // bit - Bit test
        instruction!(0x24, BIT, ZP0, 3);
        instruction!(0x2c, BIT, AB0, 4);

        // clears
        instruction!(0x18, CLC, IMP, 2);
        instruction!(0xd8, CLD, IMP, 2);
        instruction!(0x58, CLI, IMP, 2);
        instruction!(0xb8, CLV, IMP, 2);

        // cmp
        instruction!(0xc9, CMP, IMM, 2);
        instruction!(0xc5, CMP, ZP0, 3);
        instruction!(0xd5, CMP, ZPX, 4);
        instruction!(0xcd, CMP, AB0, 4);
        instruction!(0xdd, CMP, ABX, 4);
        instruction!(0xd9, CMP, ABY, 4);
        instruction!(0xc1, CMP, IDX, 6);
        instruction!(0xd1, CMP, IDY, 5);

        // cpx
        instruction!(0xe0, CPX, IMM, 2);
        instruction!(0xe4, CPX, ZP0, 3);
        instruction!(0xec, CPX, AB0, 4);

        // cpy
        instruction!(0xc0, CPY, IMM, 2);
        instruction!(0xc4, CPY, ZP0, 3);
        instruction!(0xcc, CPY, AB0, 4);

        // dec
        instruction!(0xc6, DEC, ZP0, 5);
        instruction!(0xd6, DEC, ZPX, 6);
        instruction!(0xce, DEC, AB0, 6);
        instruction!(0xde, DEC, ABX, 7);

        // decrement registers
        instruction!(0xca, DEX, IMP, 2);
        instruction!(0x88, DEY, IMP, 2);

        // eor - exclusive or
        instruction!(0x49, EOR, IMM, 2);
        instruction!(0x45, EOR, ZP0, 3);
        instruction!(0x55, EOR, ZPX, 4);
        instruction!(0x4d, EOR, AB0, 4);
        instruction!(0x5d, EOR, ABX, 4);
        instruction!(0x59, EOR, ABY, 4);
        instruction!(0x41, EOR, IDX, 6);
        instruction!(0x51, EOR, IDY, 5);

        // inc - increment memory
        instruction!(0xe6, INC, ZP0, 5);
        instruction!(0xf6, INC, ZPX, 6);
        instruction!(0xee, INC, AB0, 6);
        instruction!(0xfe, INC, ABX, 7);

        // increment registers
        instruction!(0xe8, INX, IMP, 2);
        instruction!(0xc8, INY, IMP, 2);

        // jump
        instruction!(0x4c, JMP, AB0, 3);
        instruction!(0x6c, JMP, ID0, 5);

        instruction!(0x20, JSR, AB0, 6);

        // lda = load accumulator
        instruction!(0xa9, LDA, IMM, 2);
        instruction!(0xa5, LDA, ZP0, 3);
        instruction!(0xb5, LDA, ZPX, 4);
        instruction!(0xad, LDA, AB0, 4);
        instruction!(0xbd, LDA, ABX, 4);
        instruction!(0xb9, LDA, ABY, 4);
        instruction!(0xa1, LDA, IDX, 6);
        instruction!(0xb1, LDA, IDY, 5);

        // ldx = load x register
        instruction!(0xa2, LDX, IMM, 2);
        instruction!(0xa6, LDX, ZP0, 3);
        instruction!(0xb6, LDX, ZPY, 4);
        instruction!(0xae, LDX, AB0, 4);
        instruction!(0xbe, LDX, ABY, 4);

        // ldy - load y register
        instruction!(0xa0, LDY, IMM, 2);
        instruction!(0xa4, LDY, ZP0, 3);
        instruction!(0xb4, LDY, ZPY, 4);
        instruction!(0xac, LDY, AB0, 4);
        instruction!(0xbc, LDY, ABY, 4);

        // lsr - logical shift right
        instruction!(0x4a, LSR, ACC, 2);
        instruction!(0x46, LSR, ZP0, 5);
        instruction!(0x56, LSR, ZPX, 6);
        instruction!(0x4e, LSR, AB0, 6);
        instruction!(0x5e, LSR, ABX, 7);

        // nop - no operation
        instruction!(0xea, NOP, IMP, 2);

        // ora - logical inclusive or
        instruction!(0x09, ORA, IMM, 2);
        instruction!(0x05, ORA, ZP0, 3);
        instruction!(0x15, ORA, ZPX, 4);
        instruction!(0x0d, ORA, AB0, 4);
        instruction!(0x1d, ORA, ABX, 4);
        instruction!(0x19, ORA, ABY, 4);
        instruction!(0x01, ORA, IDX, 6);
        instruction!(0x11, ORA, IDY, 5);

        // pha - push accumulator
        instruction!(0x48, PHA, IMP, 3);

        // php - push processor status
        instruction!(0x08, PHP, IMP, 3);

        // pla - pull accumulator
        instruction!(0x68, PLA, IMP, 4);

        // plp - pull processor status
        instruction!(0x28, PLP, IMP, 4);

        // rol - rotate left
        instruction!(0x2a, ROL, ACC, 2);
        instruction!(0x26, ROL, ZP0, 5);
        instruction!(0x36, ROL, ZPX, 6);
        instruction!(0x2e, ROL, AB0, 6);
        instruction!(0x3e, ROL, ABX, 7);

        // ror - rotate right
        instruction!(0x6a, ROR, ACC, 2);
        instruction!(0x66, ROR, ZP0, 5);
        instruction!(0x76, ROR, ZPX, 6);
        instruction!(0x6e, ROR, AB0, 6);
        instruction!(0x7e, ROR, ABX, 7);

        // rti - return from interrupt
        instruction!(0x40, RTI, IMP, 6);

        // rts - return from subroutine
        instruction!(0x60, RTS, IMP, 6);

        // sbc - subtract with carry
        instruction!(0xe9, SBC, IMM, 2);
        instruction!(0xe5, SBC, ZP0, 3);
        instruction!(0xf5, SBC, ZPX, 4);
        instruction!(0xed, SBC, AB0, 4);
        instruction!(0xfd, SBC, ABX, 4);
        instruction!(0xf9, SBC, ABY, 4);
        instruction!(0xe1, SBC, IDX, 6);
        instruction!(0xf1, SBC, IDY, 7);

        // sets
        instruction!(0x38, SEC, IMP, 2);
        instruction!(0xf8, SED, IMP, 2);
        instruction!(0x78, SEI, IMP, 2);

        // sta - store accumulator
        instruction!(0x85, STA, ZP0, 2);
        instruction!(0x95, STA, ZPX, 4);
        instruction!(0x8d, STA, AB0, 4);
        instruction!(0x9d, STA, ABX, 5);
        instruction!(0x99, STA, ABY, 5);
        instruction!(0x81, STA, IDX, 6);
        instruction!(0x91, STA, IDY, 6);

        // stx - store x register
        instruction!(0x86, STX, ZP0, 3);
        instruction!(0x96, STX, ZPY, 4);
        instruction!(0x8e, STX, AB0, 4);

        // sty - store y register
        instruction!(0x84, STY, ZP0, 3);
        instruction!(0x94, STY, ZPX, 4);
        instruction!(0x8c, STY, AB0, 4);

        // transfer
        instruction!(0xaa, TAX, IMP, 2); // acc -> x
        instruction!(0xa8, TAY, IMP, 2); // acc -> y
        instruction!(0xba, TSX, IMP, 2); // stack -> x
        instruction!(0x8a, TXA, IMP, 2); // x -> acc
        instruction!(0x9a, TXS, IMP, 2); // x -> stack
        instruction!(0x98, TYA, IMP, 2); // y -> acc

        res
    }

    pub const INSTRUCTIONS: &'static [Instruction; 16 * 16] = &CPU::make_instructions();

    //	add with carry
    pub fn adc(&mut self) {
        let oper = self.read_oper();

        let (mut r, c) = self.a.overflowing_add(oper);
        r += self.get_flag(Self::C) as u8;

        let v = (self.a ^ oper & !(self.a ^ r) & 0x80) != 0;

        self.set_flag(Self::V, v);
        self.set_flag(Self::C, c);
        self.set_flag(Self::Z, r == 0);
        self.set_flag(Self::N, r.get_bit(7));

        self.a = r;
    }

    //	subtract with carry
    pub fn sbc(&mut self) {
        let oper = !self.read_oper();

        let (mut r, c) = self.a.overflowing_add(oper);
        r += self.get_flag(Self::C) as u8;

        let v = (self.a ^ oper & !(self.a ^ r) & 0x80) != 0;

        self.set_flag(Self::V, v);
        self.set_flag(Self::C, c);
        self.set_flag(Self::Z, r == 0);
        self.set_flag(Self::N, r.get_bit(7));

        self.a = r;
    }
    //	and (with accumulator)
    pub fn and(&mut self) {
        self.a &= self.oper as u8;
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	arithmetic shift left
    pub fn asl(&mut self) {
        let mut v = self.read_oper();
        self.set_flag(Self::C, v.get_bit(7));
        v <<= 1;
        self.set_flag(Self::Z, v != 0);
        self.set_flag(Self::N, v.get_bit(7));

        self.write_oper(v);
    }
    //	branch on carry clear
    pub fn bcc(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::C), self.oper);
    }
    //	branch on carry set
    pub fn bcs(&mut self) {
        self.branch_on_condition(self.get_flag(Self::C), self.oper);
    }
    //	branch on equal (zero set)
    pub fn beq(&mut self) {
        self.branch_on_condition(self.get_flag(Self::Z), self.oper);
    }
    //	branch on not equal (zero clear)
    pub fn bne(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::Z), self.oper);
    }
    //	bit test
    pub fn bit(&mut self) {
        let v = self.read_oper();
        self.set_flag(Self::V, v.get_bit(6));
        self.set_flag(Self::N, v.get_bit(7));
        self.set_flag(Self::Z, self.a & v == 0);
    }
    //	branch on minus (negative set)
    pub fn bmi(&mut self) {
        self.branch_on_condition(self.get_flag(Self::N), self.oper);
    }
    //	branch on plus (negative clear)
    pub fn bpl(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::N), self.oper);
    }
    //  break / interrupt
    pub fn brk(&mut self) {
        // Set break flag
        self.set_flag(Self::B, true);

        // Write program counter to stack and dec the stack pointer
        self.bus.borrow_mut().write_u16(self.stack_addr(), self.pc);
        self.sp -= 2;

        // write status to stack and dec stack pointer
        self.bus.borrow_mut().write_u8(self.stack_addr(), self.p);
        self.sp -= 1;

        // pc is interrupt address
        self.pc = self.bus.borrow().read_u16(0xfffe);
    }
    //	branch on overflow clear
    pub fn bvc(&mut self) {
        self.branch_on_condition(!self.get_flag(Self::V), self.oper);
    }
    //	branch on overflow set
    pub fn bvs(&mut self) {
        self.branch_on_condition(self.get_flag(Self::V), self.oper);
    }
    //	clear carry
    pub fn clc(&mut self) {
        self.set_flag(Self::C, false);
    }
    //	clear decimal
    pub fn cld(&mut self) {
        self.set_flag(Self::D, false);
    }
    //	clear interrupt disable
    pub fn cli(&mut self) {
        self.set_flag(Self::I, false);
    }
    //	clear overflow
    pub fn clv(&mut self) {
        self.set_flag(Self::V, false);
    }
    //	compare (with accumulator)
    pub fn cmp(&mut self) {
        let lhs = self.a;
        let rhs = self.read_oper();
        self.compare(lhs, rhs);
    }
    //	compare with X
    pub fn cpx(&mut self) {
        let lhs = self.x;
        let rhs = self.read_oper();
        self.compare(lhs, rhs);
    }
    //	compare with Y
    pub fn cpy(&mut self) {
        let lhs = self.y;
        let rhs = self.read_oper();
        self.compare(lhs, rhs);
    }
    //	decrement
    pub fn dec(&mut self) {
        let m = self.read_oper().wrapping_sub(1);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
        self.write_oper(m);
    }
    //	decrement X
    pub fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	decrement Y
    pub fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	exclusive or (with accumulator)
    pub fn eor(&mut self) {
        self.a ^= self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	increment
    pub fn inc(&mut self) {
        let m = self.bus.borrow().read_u8(self.oper).wrapping_add(1);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
        self.write_oper(m);
    }
    //	increment X
    pub fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	increment Y
    pub fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	jump
    pub fn jmp(&mut self) {
        self.pc = self.oper;
    }
    //	jump subroutine
    pub fn jsr(&mut self) {
        self.pc = self.pc.wrapping_sub(1);
        self.bus.borrow_mut().write_u16(self.stack_addr(), self.pc);
        self.sp = self.sp.wrapping_sub(2);

        self.pc = self.oper;
    }
    //	load accumulator
    pub fn lda(&mut self) {
        self.a = self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	load X
    pub fn ldx(&mut self) {
        self.x = self.read_oper();
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	load Y
    pub fn ldy(&mut self) {
        self.y = self.read_oper();
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	logical shift right
    pub fn lsr(&mut self) {
        let mut v = self.read_oper();

        self.set_flag(Self::C, v.get_bit(0));
        v >>= 1;
        self.set_flag(Self::Z, v != 0);
        self.set_flag(Self::N, v.get_bit(7));

        self.write_oper(v);
    }
    //	no operation
    pub fn nop(&mut self) {}
    //	or with accumulator
    pub fn ora(&mut self) {
        self.a |= self.read_oper();
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	push accumulator
    pub fn pha(&mut self) {
        self.bus.borrow_mut().write_u8(self.stack_addr(), self.a);
        self.sp -= 1;
    }
    //	push processor status (SR)
    pub fn php(&mut self) {
        self.bus.borrow_mut().write_u8(self.stack_addr(), self.p);
        self.sp -= 1;
    }
    //	pull accumulator
    pub fn pla(&mut self) {
        self.a = self.bus.borrow().read_u8(self.stack_addr());
        self.set_flag(Self::Z, self.a != 0);
        self.set_flag(Self::N, self.a.get_bit(7));
        self.sp += 1;
    }
    //	pull processor status (SR)
    pub fn plp(&mut self) {
        self.p = self.bus.borrow().read_u8(self.stack_addr());
        self.sp += 1;
    }
    //	rotate left
    pub fn rol(&mut self) {
        let old = self.read_oper();
        let mut new = old << 1;
        new |= self.p & 0x1;

        self.set_flag(Self::C, old.get_bit(0));
        self.set_flag(Self::Z, new == 0);
        self.set_flag(Self::N, new.get_bit(7));

        self.write_oper(new);
    }
    //	rotate right
    pub fn ror(&mut self) {
        let old = self.read_oper();
        let mut new = old >> 1;
        new |= self.p & 0x80;

        self.set_flag(Self::C, old.get_bit(0));
        self.set_flag(Self::Z, new == 0);
        self.set_flag(Self::N, new.get_bit(7));

        self.write_oper(new);
    }
    //	return from interrupt
    pub fn rti(&mut self) {}
    //	return from subroutine
    pub fn rts(&mut self) {
        self.pc = self
            .bus
            .borrow()
            .read_u16(self.stack_addr())
            .wrapping_add(1);
        self.sp = self.sp.wrapping_add(2);
    }
    //	set carry
    pub fn sec(&mut self) {
        self.set_flag(Self::C, true);
    }
    //	set decimal
    pub fn sed(&mut self) {
        self.set_flag(Self::D, true);
    }
    //	set interrupt disable
    pub fn sei(&mut self) {
        self.set_flag(Self::I, true);
    }
    //	store accumulator
    pub fn sta(&mut self) {
        self.bus.borrow_mut().write_u8(self.oper, self.a);
    }
    //	store X
    pub fn stx(&mut self) {
        self.bus.borrow_mut().write_u8(self.oper, self.x);
    }
    //	store Y
    pub fn sty(&mut self) {
        self.bus.borrow_mut().write_u8(self.oper, self.y);
    }
    //	transfer accumulator to X
    pub fn tax(&mut self) {
        self.x = self.a;
    }
    //	transfer accumulator to Y
    pub fn tay(&mut self) {
        self.y = self.a
    }
    //	transfer stack pointer to X
    pub fn tsx(&mut self) {
        self.x = self.sp;
    }
    //	transfer X to accumulator
    pub fn txa(&mut self) {
        self.a = self.x;
    }
    //	transfer X to stack pointer
    pub fn txs(&mut self) {
        self.sp = self.x;
    }
    //	transfer Y to accumulator
    pub fn tya(&mut self) {
        self.a = self.y;
    }
}
