use super::address_mode::AddressMode;
use super::operation::Operation;
use super::CPU;

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

    pub fn is_valid(&self) -> bool {
        !(self.addr_mode == AddressMode::XXX || self.operation == Operation::XXX)
    }
}

impl CPU {
    pub const INSTRUCTIONS: [Instruction; 16 * 16] = {
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
        instruction!(0x75, ADC, ZPX, 4);
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
        instruction!(0xa8, TAY, IMP, 2); // acc -> y
        instruction!(0x98, TYA, IMP, 2); // y -> acc
        instruction!(0xba, TSX, IMP, 2); // stack -> x
        instruction!(0x9a, TXS, IMP, 2); // x -> stack
        instruction!(0xaa, TAX, IMP, 2); // acc -> x
        instruction!(0x8a, TXA, IMP, 2); // x -> acc

        res
    };
}
