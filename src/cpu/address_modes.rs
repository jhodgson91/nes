use super::CPU;
use bit_field::BitField;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum AddressMode {
    ACC, //	Accumulator	 	        OPC A	 	    operand is AC (implied single byte instruction)
    AB0, //	absolute	 	        OPC $LLHH	 	operand is address $HHLL *
    ABX, //	absolute, X-indexed	 	OPC $LLHH,X	 	operand is address; effective address is address incremented by X with carry **
    ABY, //	absolute, Y-indexed	 	OPC $LLHH,Y	 	operand is address; effective address is address incremented by Y with carry **
    IMM, //	immediate	 	        OPC #$BB	 	operand is byte BB
    IMP, //	implied	 	            OPC	 	        operand implied
    ID0, //	indirect	 	        OPC ($LLHH)	 	operand is address; effective address is contents of word at address: C.w($HHLL)
    IDX, //	X-indexed, indirect	 	OPC ($LL,X)	 	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    IDY, //	indirect, Y-indexed	 	OPC ($LL),Y	 	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    REL, //	relative	 	        OPC $BB	 	    branch target is PC + signed offset BB ***
    ZP0, //	zeropage	 	        OPC $LL	 	    operand is zeropage address (hi-byte is zero, address = $00LL)
    ZPX, //	zeropage, X-indexed	 	OPC $LL,X	 	operand is zeropage address; effective address is address incremented by X without carry **
    ZPY, //	zeropage, Y-indexed	 	OPC $LL,Y	 	operand is zeropage address; effective address is address incremented by Y without carry **
    XXX, // invalid
}

impl AddressMode {
    pub fn method(&self) -> fn(&mut CPU) {
        match self {
            AddressMode::ACC => CPU::acc,
            AddressMode::AB0 => CPU::ab0,
            AddressMode::ABX => CPU::abx,
            AddressMode::ABY => CPU::aby,
            AddressMode::IMM => CPU::imm,
            AddressMode::IMP => CPU::imp,
            AddressMode::ID0 => CPU::id0,
            AddressMode::IDX => CPU::idx,
            AddressMode::IDY => CPU::idy,
            AddressMode::REL => CPU::rel,
            AddressMode::ZP0 => CPU::zp0,
            AddressMode::ZPX => CPU::zpx,
            AddressMode::ZPY => CPU::zpy,
            AddressMode::XXX => CPU::xxx,
        }
    }
}

impl CPU {
    // Addressing modes
    // The CPU has already read and incremented pc for the opcode

    //....	zeropage	 	        OPC $LL	 	    operand is zeropage address (hi-byte is zero, address = $00LL)
    pub fn zp0(&mut self) {
        let addr = self.bus.borrow().cpu_read::<u8>(self.pc) as u16;
        self.pc += 1;

        self.oper = addr;
    }
    //....	zeropage, X-indexed	 	OPC $LL,X	 	operand is zeropage address; effective address is address incremented by X without carry **
    pub fn zpx(&mut self) {
        let addr = (self
            .bus
            .borrow()
            .cpu_read::<u8>(self.pc)
            .wrapping_add(self.x)) as u16;
        self.pc += 1;
        self.oper = addr;
    }
    //....	zeropage, Y-indexed	 	OPC $LL,Y	 	operand is zeropage address; effective address is address incremented by Y without carry **
    pub fn zpy(&mut self) {
        let addr = (self
            .bus
            .borrow()
            .cpu_read::<u8>(self.pc)
            .wrapping_add(self.y)) as u16;
        self.pc += 1;
        self.oper = addr;
    }
    //....	absolute	 	        OPC $LLHH	 	operand is address $HHLL *
    pub fn ab0(&mut self) {
        let addr = self.bus.borrow().cpu_read::<u16>(self.pc);
        self.pc += 2;
        self.oper = addr;
    }
    //....	absolute, X-indexed	 	OPC $LLHH,X	 	operand is address; effective address is address incremented by X with carry **
    pub fn abx(&mut self) {
        let ptr = self.bus.borrow().cpu_read::<u16>(self.pc);
        self.pc += 2;
        let addr = ptr + self.x as u16;

        self.instruction.cycles = if addr & 0xff00 == ptr & 0xff00 { 0 } else { 1 };
        self.oper = addr;
    }
    //....	absolute, Y-indexed	 	OPC $LLHH,Y	 	operand is address; effective address is address incremented by Y with carry **
    pub fn aby(&mut self) {
        let ptr = self.bus.borrow().cpu_read::<u16>(self.pc);
        self.pc += 2;
        let addr = ptr + self.y as u16;

        self.instruction.cycles = if addr & 0xff00 == ptr & 0xff00 { 0 } else { 1 };
        self.oper = addr;
    }
    //....	Accumulator	 	        OPC A	 	    operand is AC (implied single byte instruction)
    pub fn acc(&mut self) {
        self.oper = self.a as u16;
    }
    //....	immediate	 	        OPC #$BB	 	operand is byte BB
    pub fn imm(&mut self) {
        self.oper = self.bus.borrow().cpu_read::<u8>(self.pc) as u16;
        self.pc += 1;
    }
    //....	implied	 	            OPC	 	        operand implied
    pub fn imp(&mut self) {}
    //....	indirect	 	        OPC ($LLHH)	 	operand is address; effective address is contents of word at address: C.w($HHLL)
    pub fn id0(&mut self) {
        let ptr = self.bus.borrow().cpu_read::<u16>(self.pc);
        self.oper = self.bus.borrow().cpu_read(ptr);
        self.pc += 2;
    }
    //....	X-indexed, indirect	 	OPC ($LL,X)	 	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    pub fn idx(&mut self) {
        let ptr = self
            .bus
            .borrow()
            .cpu_read::<u8>(self.pc)
            .wrapping_add(self.x) as u16;
        self.oper = self.bus.borrow().cpu_read(ptr);
        self.pc += 1;
    }
    //....	indirect, Y-indexed	 	OPC ($LL),Y	 	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    pub fn idy(&mut self) {
        let ptr = self.bus.borrow().cpu_read::<u8>(self.pc) as u16;
        self.pc += 1;

        self.oper = self
            .bus
            .borrow()
            .cpu_read::<u16>(ptr)
            .wrapping_add(self.y as u16);
        self.instruction.cycles += if ptr & 0xff00 == self.oper & 0xff00 {
            0
        } else {
            1
        };
    }
    //....	relative	 	        OPC $BB	 	    branch target is PC + signed offset BB ***
    pub fn rel(&mut self) {
        self.oper = self.bus.borrow().cpu_read::<u8>(self.pc) as u16;
        if self.oper.get_bit(7) {
            self.oper |= 0xff00;
        }
        self.pc += 1;
    }
}
