use super::address_modes::AddressMode;
use super::CPU;

use bit_field::BitField;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum InstructionCode {
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

impl InstructionCode {
    pub fn method(&self) -> impl Fn(&mut CPU) {
        match self {
            InstructionCode::ADC => CPU::adc,
            InstructionCode::AND => CPU::and,
            InstructionCode::ASL => CPU::asl,
            InstructionCode::BCC => CPU::bcc,
            InstructionCode::BCS => CPU::bcs,
            InstructionCode::BEQ => CPU::beq,
            InstructionCode::BIT => CPU::bit,
            InstructionCode::BMI => CPU::bmi,
            InstructionCode::BNE => CPU::bne,
            InstructionCode::BPL => CPU::bpl,
            InstructionCode::BRK => CPU::brk,
            InstructionCode::BVC => CPU::bvc,
            InstructionCode::BVS => CPU::bvs,
            InstructionCode::CLC => CPU::clc,
            InstructionCode::CLD => CPU::cld,
            InstructionCode::CLI => CPU::cli,
            InstructionCode::CLV => CPU::clv,
            InstructionCode::CMP => CPU::cmp,
            InstructionCode::CPX => CPU::cpx,
            InstructionCode::CPY => CPU::cpy,
            InstructionCode::DEC => CPU::dec,
            InstructionCode::DEX => CPU::dex,
            InstructionCode::DEY => CPU::dey,
            InstructionCode::EOR => CPU::eor,
            InstructionCode::INC => CPU::inc,
            InstructionCode::INX => CPU::inx,
            InstructionCode::INY => CPU::iny,
            InstructionCode::JMP => CPU::jmp,
            InstructionCode::JSR => CPU::jsr,
            InstructionCode::LDA => CPU::lda,
            InstructionCode::LDX => CPU::ldx,
            InstructionCode::LDY => CPU::ldy,
            InstructionCode::LSR => CPU::lsr,
            InstructionCode::NOP => CPU::nop,
            InstructionCode::ORA => CPU::ora,
            InstructionCode::PHA => CPU::pha,
            InstructionCode::PHP => CPU::php,
            InstructionCode::PLA => CPU::pla,
            InstructionCode::PLP => CPU::plp,
            InstructionCode::ROL => CPU::rol,
            InstructionCode::ROR => CPU::ror,
            InstructionCode::RTI => CPU::rti,
            InstructionCode::RTS => CPU::rts,
            InstructionCode::SBC => CPU::sbc,
            InstructionCode::SEC => CPU::sec,
            InstructionCode::SED => CPU::sed,
            InstructionCode::SEI => CPU::sei,
            InstructionCode::STA => CPU::sta,
            InstructionCode::STX => CPU::stx,
            InstructionCode::STY => CPU::sty,
            InstructionCode::TAX => CPU::tax,
            InstructionCode::TAY => CPU::tay,
            InstructionCode::TSX => CPU::tsx,
            InstructionCode::TXA => CPU::txa,
            InstructionCode::TXS => CPU::txs,
            InstructionCode::TYA => CPU::tya,
            InstructionCode::XXX => CPU::xxx,
        }
    }
}

type InstructionFn = &'static dyn FnMut(&mut CPU);

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub name: &'static str,
    pub code: InstructionCode,
    pub addr_mode: AddressMode,
    pub cycles: u8,
}

impl Instruction {
    pub const INVALID: Instruction = Instruction {
        name: "???",
        code: InstructionCode::XXX,
        addr_mode: AddressMode::XXX,
        cycles: 0,
    };
}

macro_rules! instruction {
    ($ins:ident, $addr: ident, $cycles:expr) => {
        Instruction {
            name: stringify!($ins),
            code: InstructionCode::$ins,
            addr_mode: AddressMode::$addr,
            cycles: $cycles,
        }
    };
}

impl CPU {
    const fn make_instructions() -> [[Instruction; 16]; 16] {
        let mut res = [[Instruction::INVALID; 16]; 16];

        // adc
        res[0x6][0x9] = instruction!(ADC, IMM, 2);
        res[0x6][0x5] = instruction!(ADC, ZP0, 3);
        res[0x6][0x9] = instruction!(ADC, ZPX, 4);
        res[0x6][0xd] = instruction!(ADC, AB0, 4);
        res[0x7][0xd] = instruction!(ADC, ABX, 4);
        res[0x7][0x9] = instruction!(ADC, ABY, 4);
        res[0x6][0x1] = instruction!(ADC, IDX, 6);
        res[0x7][0x1] = instruction!(ADC, IDY, 5);

        // AND - Logical AND
        res[0x2][0x9] = instruction!(AND, IMM, 2);
        res[0x2][0x5] = instruction!(AND, ZP0, 3);
        res[0x3][0x5] = instruction!(AND, ZPX, 4);
        res[0x2][0xd] = instruction!(AND, AB0, 4);
        res[0x3][0xd] = instruction!(AND, ABX, 4);
        res[0x3][0x9] = instruction!(AND, ABY, 4);
        res[0x2][0x1] = instruction!(AND, IDX, 6);
        res[0x3][0x1] = instruction!(AND, IDY, 5);

        // asl - arithmetic shift left
        res[0x0][0xa] = instruction!(ASL, ACC, 2);
        res[0x0][0x6] = instruction!(ASL, ZP0, 5);
        res[0x1][0x6] = instruction!(ASL, ZPX, 6);
        res[0x0][0xe] = instruction!(ASL, AB0, 6);
        res[0x1][0xe] = instruction!(ASL, ABX, 7);

        // branches
        res[0x9][0x0] = instruction!(BCC, REL, 2);
        res[0xb][0x0] = instruction!(BCS, REL, 2);
        res[0xf][0x0] = instruction!(BEQ, REL, 2);
        res[0x3][0x0] = instruction!(BMI, REL, 2);
        res[0xd][0x0] = instruction!(BNE, REL, 2);
        res[0x1][0x0] = instruction!(BPL, REL, 2);
        res[0x5][0x0] = instruction!(BVC, REL, 2);
        res[0x7][0x0] = instruction!(BVS, REL, 2);

        // BRK - Force Interrupt
        res[0x0][0x0] = instruction!(BRK, IMP, 7);

        // bit - Bit test
        res[0x2][0x4] = instruction!(BIT, ZP0, 3);
        res[0x2][0xc] = instruction!(BIT, AB0, 4);

        // clears
        res[0x1][0x8] = instruction!(CLC, IMP, 2);
        res[0xd][0x8] = instruction!(CLD, IMP, 2);
        res[0x5][0x8] = instruction!(CLI, IMP, 2);
        res[0xb][0x8] = instruction!(CLV, IMP, 2);

        // cmp
        res[0xc][0x9] = instruction!(CMP, IMM, 2);
        res[0xc][0x5] = instruction!(CMP, ZP0, 3);
        res[0xd][0x5] = instruction!(CMP, ZPX, 4);
        res[0xc][0xd] = instruction!(CMP, AB0, 4);
        res[0xd][0xd] = instruction!(CMP, ABX, 4);
        res[0xd][0x9] = instruction!(CMP, ABY, 4);
        res[0xc][0x1] = instruction!(CMP, IDX, 6);
        res[0xd][0x1] = instruction!(CMP, IDY, 5);

        // cpx
        res[0xe][0x0] = instruction!(CPX, IMM, 2);
        res[0xe][0x4] = instruction!(CPX, ZP0, 3);
        res[0xe][0xc] = instruction!(CPX, AB0, 4);

        // cpy
        res[0xc][0x0] = instruction!(CPY, IMM, 2);
        res[0xc][0x4] = instruction!(CPY, ZP0, 3);
        res[0xc][0xc] = instruction!(CPY, AB0, 4);

        // dec
        res[0xc][0x6] = instruction!(DEC, ZP0, 5);
        res[0xd][0x6] = instruction!(DEC, ZPX, 6);
        res[0xc][0xe] = instruction!(DEC, AB0, 6);
        res[0xd][0xe] = instruction!(DEC, ABX, 7);

        // decrement registers
        res[0xc][0xa] = instruction!(DEX, IMP, 2);
        res[0x8][0x8] = instruction!(DEY, IMP, 2);

        // eor - exclusive or
        res[0x4][0x9] = instruction!(EOR, IMM, 2);
        res[0x4][0x5] = instruction!(EOR, ZP0, 3);
        res[0x5][0x5] = instruction!(EOR, ZPX, 4);
        res[0x4][0xd] = instruction!(EOR, AB0, 4);
        res[0x5][0xd] = instruction!(EOR, ABX, 4);
        res[0x5][0x9] = instruction!(EOR, ABY, 4);
        res[0x4][0x1] = instruction!(EOR, IDX, 6);
        res[0x5][0x1] = instruction!(EOR, IDY, 5);

        // inc - increment memory
        res[0xe][0x6] = instruction!(INC, ZP0, 5);
        res[0xf][0x6] = instruction!(INC, ZPX, 6);
        res[0xe][0xe] = instruction!(INC, AB0, 6);
        res[0xf][0xe] = instruction!(INC, ABX, 7);

        // increment registers
        res[0xe][0x8] = instruction!(INX, IMP, 2);
        res[0xc][0x8] = instruction!(INY, IMP, 2);

        // jump
        res[0x4][0xc] = instruction!(JMP, AB0, 3);
        res[0x6][0xc] = instruction!(JMP, ID0, 5);

        res[0x2][0x0] = instruction!(JSR, AB0, 6);

        // lda = load accumulator
        res[0xa][0x9] = instruction!(LDA, IMM, 2);
        res[0xa][0x5] = instruction!(LDA, ZP0, 3);
        res[0xb][0x5] = instruction!(LDA, ZPX, 4);
        res[0xa][0xd] = instruction!(LDA, AB0, 4);
        res[0xb][0xd] = instruction!(LDA, ABX, 4);
        res[0xb][0x9] = instruction!(LDA, ABY, 4);
        res[0xa][0x1] = instruction!(LDA, IDX, 6);
        res[0xb][0x1] = instruction!(LDA, IDY, 5);

        // ldx = load x register
        res[0xa][0x2] = instruction!(LDX, IMM, 2);
        res[0xa][0x6] = instruction!(LDX, ZP0, 3);
        res[0xb][0x6] = instruction!(LDX, ZPY, 4);
        res[0xa][0xe] = instruction!(LDX, AB0, 4);
        res[0xb][0xe] = instruction!(LDX, ABY, 4);

        // ldy - load y register
        res[0xa][0x0] = instruction!(LDY, IMM, 2);
        res[0xa][0x4] = instruction!(LDY, ZP0, 3);
        res[0xb][0x4] = instruction!(LDY, ZPY, 4);
        res[0xa][0xc] = instruction!(LDY, AB0, 4);
        res[0xb][0xc] = instruction!(LDY, ABY, 4);

        // lsr - logical shift right
        res[0x4][0xa] = instruction!(LSR, ACC, 2);
        res[0x4][0x6] = instruction!(LSR, ZP0, 5);
        res[0x5][0x6] = instruction!(LSR, ZPX, 6);
        res[0x4][0xe] = instruction!(LSR, AB0, 6);
        res[0x5][0xe] = instruction!(LSR, ABX, 7);

        // nop - no operation
        res[0xe][0xa] = instruction!(NOP, IMP, 2);

        // ora - logical inclusive or
        res[0x0][0x9] = instruction!(ORA, IMM, 2);
        res[0x0][0x5] = instruction!(ORA, ZP0, 3);
        res[0x1][0x5] = instruction!(ORA, ZPX, 4);
        res[0x0][0xd] = instruction!(ORA, AB0, 4);
        res[0x1][0xd] = instruction!(ORA, ABX, 4);
        res[0x1][0x9] = instruction!(ORA, ABY, 4);
        res[0x0][0x1] = instruction!(ORA, IDX, 6);
        res[0x1][0x1] = instruction!(ORA, IDY, 5);

        // pha - push accumulator
        res[0x4][0x8] = instruction!(PHA, IMP, 3);

        // php - push processor status
        res[0x0][0x8] = instruction!(PHP, IMP, 3);

        // pla - pull accumulator
        res[0x6][0x8] = instruction!(PLA, IMP, 4);

        // plp - pull processor status
        res[0x2][0x8] = instruction!(PLP, IMP, 4);

        // rol - rotate left
        res[0x2][0xa] = instruction!(ROL, ACC, 2);
        res[0x2][0x6] = instruction!(ROL, ZP0, 5);
        res[0x3][0x6] = instruction!(ROL, ZPX, 6);
        res[0x2][0xe] = instruction!(ROL, AB0, 6);
        res[0x3][0xe] = instruction!(ROL, ABX, 7);

        // ror - rotate right
        res[0x6][0xa] = instruction!(ROR, ACC, 2);
        res[0x6][0x6] = instruction!(ROR, ZP0, 5);
        res[0x7][0x6] = instruction!(ROR, ZPX, 6);
        res[0x6][0xe] = instruction!(ROR, AB0, 6);
        res[0x7][0xe] = instruction!(ROR, ABX, 7);

        // rti - return from interrupt
        res[0x4][0x0] = instruction!(RTI, IMP, 6);

        // rts - return from subroutine
        res[0x6][0x0] = instruction!(RTS, IMP, 6);

        // sbc - subtract with carry
        res[0xe][0x9] = instruction!(SBC, IMM, 2);
        res[0xe][0x5] = instruction!(SBC, ZP0, 3);
        res[0xf][0x5] = instruction!(SBC, ZPX, 4);
        res[0xe][0xd] = instruction!(SBC, AB0, 4);
        res[0xf][0xd] = instruction!(SBC, ABX, 4);
        res[0xf][0x9] = instruction!(SBC, ABY, 4);
        res[0xe][0x1] = instruction!(SBC, IDX, 6);
        res[0xf][0x1] = instruction!(SBC, IDY, 7);

        // sets
        res[0x3][0x8] = instruction!(SEC, IMP, 2);
        res[0xf][0x8] = instruction!(SED, IMP, 2);
        res[0x7][0x8] = instruction!(SEI, IMP, 2);

        // sta - store accumulator
        res[0x8][0x5] = instruction!(STA, ZP0, 2);
        res[0x9][0x5] = instruction!(STA, ZPX, 4);
        res[0x8][0xd] = instruction!(STA, AB0, 4);
        res[0x9][0xd] = instruction!(STA, ABX, 5);
        res[0x9][0x9] = instruction!(STA, ABY, 5);
        res[0x8][0x1] = instruction!(STA, IDX, 6);
        res[0x9][0x1] = instruction!(STA, IDY, 6);

        // stx - store x register
        res[0x8][0x6] = instruction!(STX, ZP0, 3);
        res[0x9][0x6] = instruction!(STX, ZPY, 4);
        res[0x8][0xe] = instruction!(STX, AB0, 4);

        // sty - store y register
        res[0x8][0x4] = instruction!(STX, ZP0, 3);
        res[0x9][0x4] = instruction!(STX, ZPX, 4);
        res[0x8][0xc] = instruction!(STX, AB0, 4);

        // transfer
        res[0xa][0xa] = instruction!(TAX, IMP, 2); // acc -> x
        res[0xa][0x8] = instruction!(TAY, IMP, 2); // acc -> y
        res[0xb][0x8] = instruction!(TSX, IMP, 2); // stack -> x
        res[0x8][0xa] = instruction!(TXA, IMP, 2); // x -> acc
        res[0x9][0xa] = instruction!(TXS, IMP, 2); // x -> stack
        res[0x9][0x8] = instruction!(TYA, IMP, 2); // y -> acc

        res
    }

    pub const INSTRUCTIONS: &'static [[Instruction; 16]; 16] = &CPU::make_instructions();

    //	add with carry
    pub fn adc(&mut self) {}
    //	and (with accumulator)
    pub fn and(&mut self) {
        self.a &= self.oper as u8;
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	arithmetic shift left
    pub fn asl(&mut self) {
        if self.instruction.addr_mode == AddressMode::ACC {
            self.set_flag(Self::C, self.a.get_bit(7));
            self.a <<= 1;
            self.set_flag(Self::Z, self.a != 0);
            self.set_flag(Self::N, self.a.get_bit(7));
        } else {
            let mut v = self.bus.borrow_mut().read_u8(self.oper);
            self.set_flag(Self::C, v.get_bit(7));
            v <<= 1;
            self.bus.borrow_mut().write_u8(self.oper, v);
            self.set_flag(Self::Z, v != 0);
            self.set_flag(Self::N, v.get_bit(7));
        };
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
        let v = self.bus.borrow().read_u8(self.oper);
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
        self.bus
            .borrow_mut()
            .write_u16(Self::STACK + self.s as u16, self.pc);
        self.s -= 2;

        // write status to stack and dec stack pointer
        self.bus
            .borrow_mut()
            .write_u8(Self::STACK + self.s as u16, self.p);
        self.s -= 1;

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
        let rhs = if self.instruction.addr_mode == AddressMode::IMM {
            self.oper as u8
        } else {
            self.bus.borrow().read_u8(self.oper)
        };
        self.compare(lhs, rhs);
    }
    //	compare with X
    pub fn cpx(&mut self) {
        let lhs = self.x;
        let rhs = if self.instruction.addr_mode == AddressMode::IMM {
            self.oper as u8
        } else {
            self.bus.borrow().read_u8(self.oper)
        };
        self.compare(lhs, rhs);
    }
    //	compare with Y
    pub fn cpy(&mut self) {
        let lhs = self.y;
        let rhs = if self.instruction.addr_mode == AddressMode::IMM {
            self.oper as u8
        } else {
            self.bus.borrow().read_u8(self.oper)
        };
        self.compare(lhs, rhs);
    }
    //	decrement
    pub fn dec(&mut self) {
        let m = self.bus.borrow().read_u8(self.oper) - 1;
        self.bus.borrow_mut().write_u8(self.oper, m);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
    }
    //	decrement X
    pub fn dex(&mut self) {
        self.x -= 1;
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	decrement Y
    pub fn dey(&mut self) {
        self.y -= 1;
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	exclusive or (with accumulator)
    pub fn eor(&mut self) {
        if self.instruction.addr_mode == AddressMode::IMM {
            self.a ^= self.oper as u8;
        } else {
            self.a ^= self.bus.borrow().read_u8(self.oper);
        }
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	increment
    pub fn inc(&mut self) {
        let m = self.bus.borrow().read_u8(self.oper) + 1;
        self.bus.borrow_mut().write_u8(self.oper, m);
        self.set_flag(Self::Z, m == 0);
        self.set_flag(Self::N, m.get_bit(7));
    }
    //	increment X
    pub fn inx(&mut self) {
        self.x += 1;
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	increment Y
    pub fn iny(&mut self) {
        self.y += 1;
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	jump
    pub fn jmp(&mut self) {
        self.pc = self.oper;
    }
    //	jump subroutine
    pub fn jsr(&mut self) {}
    //	load accumulator
    pub fn lda(&mut self) {
        self.a = self.bus.borrow().read_u8(self.oper);
        self.set_flag(Self::Z, self.a == 0);
        self.set_flag(Self::N, self.a.get_bit(7));
    }
    //	load X
    pub fn ldx(&mut self) {
        self.x = self.bus.borrow().read_u8(self.oper);
        self.set_flag(Self::Z, self.x == 0);
        self.set_flag(Self::N, self.x.get_bit(7));
    }
    //	load Y
    pub fn ldy(&mut self) {
        self.y = self.bus.borrow().read_u8(self.oper);
        self.set_flag(Self::Z, self.y == 0);
        self.set_flag(Self::N, self.y.get_bit(7));
    }
    //	logical shift right
    pub fn lsr(&mut self) {
        let mut v = if self.instruction.addr_mode == AddressMode::ACC {
            self.a
        } else {
            self.bus.borrow_mut().read_u8(self.oper)
        };

        self.set_flag(Self::C, v.get_bit(0));
        v >>= 1;
        self.set_flag(Self::Z, v != 0);
        self.set_flag(Self::N, v.get_bit(7));

        if self.instruction.addr_mode == AddressMode::ACC {
            self.a = v;
        } else {
            self.bus.borrow_mut().write_u8(self.oper, v);
        };
    }
    //	no operation
    pub fn nop(&mut self) {}
    //	or with accumulator
    pub fn ora(&mut self) {
        if self.instruction.addr_mode == AddressMode::IMM {
            self.a |= self.oper as u8;
            self.set_flag(Self::Z, self.a == 0);
            self.set_flag(Self::N, self.a.get_bit(7));
        }
    }
    //	push accumulator
    pub fn pha(&mut self) {
        self.bus
            .borrow_mut()
            .write_u8(Self::STACK + self.s as u16, self.a);
        self.s -= 1;
    }
    //	push processor status (SR)
    pub fn php(&mut self) {
        self.bus
            .borrow_mut()
            .write_u8(Self::STACK + self.s as u16, self.p);
        self.s -= 1;
    }
    //	pull accumulator
    pub fn pla(&mut self) {
        self.a = self.bus.borrow().read_u8(Self::STACK + self.s as u16);
        self.set_flag(Self::Z, self.a != 0);
        self.set_flag(Self::N, self.a.get_bit(7));
        self.s += 1;
    }
    //	pull processor status (SR)
    pub fn plp(&mut self) {
        self.p = self.bus.borrow().read_u8(Self::STACK + self.s as u16);
        self.s += 1;
    }
    //	rotate left
    pub fn rol(&mut self) {
        if self.instruction.addr_mode == AddressMode::ACC {
            let mut new = self.a << 1;
            new |= self.p & 0x1;
            self.set_flag(Self::C, self.a.get_bit(7));
            self.set_flag(Self::Z, new == 0);
            self.set_flag(Self::N, new.get_bit(7));
            self.a = new;
        } else {
            let old = self.bus.borrow().read_u8(self.oper);
            let mut new = old << 1;
            new |= self.p & 0x1;

            self.set_flag(Self::C, old.get_bit(7));
            self.set_flag(Self::Z, new == 0);
            self.set_flag(Self::N, new.get_bit(7));
            self.bus.borrow_mut().write_u8(self.oper, new);
        }
    }
    //	rotate right
    pub fn ror(&mut self) {
        if self.instruction.addr_mode == AddressMode::ACC {
            let mut new = self.a >> 1;
            new |= (self.p & 0x1) << 7;

            self.set_flag(Self::C, self.a.get_bit(0));
            self.set_flag(Self::Z, new == 0);
            self.set_flag(Self::N, new.get_bit(7));
            self.a = new;
        } else {
            let old = self.bus.borrow().read_u8(self.oper);
            let mut new = old >> 1;
            new |= (self.p & 0x1) << 7;

            self.set_flag(Self::C, old.get_bit(0));
            self.set_flag(Self::Z, new == 0);
            self.set_flag(Self::N, new.get_bit(7));
            self.bus.borrow_mut().write_u8(self.oper, new);
        }
    }
    //	return from interrupt
    pub fn rti(&mut self) {}
    //	return from subroutine
    pub fn rts(&mut self) {}
    //	subtract with carry
    pub fn sbc(&mut self) {}
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
        self.x = self.s;
    }
    //	transfer X to accumulator
    pub fn txa(&mut self) {
        self.a = self.x;
    }
    //	transfer X to stack pointer
    pub fn txs(&mut self) {
        self.s = self.x;
    }
    //	transfer Y to accumulator
    pub fn tya(&mut self) {
        self.a = self.y;
    }
    // invalid instruction
    pub fn xxx(&mut self) {}
}
