use super::CPU;

/*
Address Modes:
ACC 	    ....	Accumulator	 	        OPC A	 	    operand is AC (implied single byte instruction)
ABS		    ....	absolute	 	        OPC $LLHH	 	operand is address $HHLL *
ABX		....	absolute, X-indexed	 	OPC $LLHH,X	 	operand is address; effective address is address incremented by X with carry **
ABY		....	absolute, Y-indexed	 	OPC $LLHH,Y	 	operand is address; effective address is address incremented by Y with carry **
IMM		    ....	immediate	 	        OPC #$BB	 	operand is byte BB
IMP		....	implied	 	            OPC	 	        operand implied
ID0		    ....	indirect	 	        OPC ($LLHH)	 	operand is address; effective address is contents of word at address: C.w($HHLL)
IDX		....	X-indexed, indirect	 	OPC ($LL,X)	 	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
IDY		....	indirect, Y-indexed	 	OPC ($LL),Y	 	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
REL		    ....	relative	 	        OPC $BB	 	    branch target is PC + signed offset BB ***
ZP0		    ....	zeropage	 	        OPC $LL	 	    operand is zeropage address (hi-byte is zero, address = $00LL)
ZPX		....	zeropage, X-indexed	 	OPC $LL,X	 	operand is zeropage address; effective address is address incremented by X without carry **
ZPY		....	zeropage, Y-indexed	 	OPC $LL,Y	 	operand is zeropage address; effective address is address incremented by Y without carry **
*/

impl CPU {
    // Addressing modes
    // The CPU has alread read and incremented pc for the opcode
    // These now, from the operand, resolve to the resolved operand of the instruction
    // This resolved operand is either an absolute address, or a constant
    // return tuple of (resolved operand, extra cycles needed)

    //....	zeropage	 	        OPC $LL	 	    operand is zeropage address (hi-byte is zero, address = $00LL)
    pub fn zp0(&mut self) -> (u16, u8) {
        let addr = self.bus.borrow().read_u8(self.pc) as u16;
        self.pc += 1;
        (addr, 0)
    }
    //....	zeropage, X-indexed	 	OPC $LL,X	 	operand is zeropage address; effective address is address incremented by X without carry **
    pub fn zpx(&mut self) -> (u16, u8) {
        let addr = (self.bus.borrow().read_u8(self.pc).wrapping_add(self.x)) as u16;
        self.pc += 1;
        (addr, 0)
    }
    //....	zeropage, Y-indexed	 	OPC $LL,Y	 	operand is zeropage address; effective address is address incremented by Y without carry **
    pub fn zpy(&mut self) -> (u16, u8) {
        let addr = (self.bus.borrow().read_u8(self.pc).wrapping_add(self.y)) as u16;
        self.pc += 1;
        (addr, 0)
    }
    //....	absolute	 	        OPC $LLHH	 	operand is address $HHLL *
    pub fn ab0(&mut self) -> (u16, u8) {
        let addr = self.bus.borrow().read_u16(self.pc);
        self.pc += 2;
        (addr, 0)
    }
    //....	absolute, X-indexed	 	OPC $LLHH,X	 	operand is address; effective address is address incremented by X with carry **
    pub fn abx(&mut self) -> (u16, u8) {
        let ptr = self.bus.borrow().read_u16(self.pc);
        self.pc += 2;
        let addr = ptr + self.x as u16;
        let cycles = if addr & 0xff00 == ptr & 0xff00 { 0 } else { 1 };
        (addr, cycles)
    }
    //....	absolute, Y-indexed	 	OPC $LLHH,Y	 	operand is address; effective address is address incremented by Y with carry **
    pub fn aby(&mut self) -> (u16, u8) {
        let ptr = self.bus.borrow().read_u16(self.pc);
        self.pc += 2;
        let addr = ptr + self.x as u16;
        let cycles = if addr & 0xff00 == ptr & 0xff00 { 0 } else { 1 };
        (addr, cycles)
    }
    //....	Accumulator	 	        OPC A	 	    operand is AC (implied single byte instruction)
    pub fn acc(&mut self) -> (u16, u8) {
        (self.a as u16, 0)
    }
    //....	immediate	 	        OPC #$BB	 	operand is byte BB
    pub fn imm(&mut self) -> (u16, u8) {
        let oper = self.bus.borrow().read_u8(self.pc) as u16;
        self.pc += 1;
        (oper, 0)
    }
    //....	implied	 	            OPC	 	        operand implied
    pub fn imp(&mut self) -> (u16, u8) {
        (0, 0)
    }
    //....	indirect	 	        OPC ($LLHH)	 	operand is address; effective address is contents of word at address: C.w($HHLL)
    pub fn id0(&mut self) -> (u16, u8) {
        let ptr = self.bus.borrow().read_u16(self.pc);
        self.pc += 2;
        (self.bus.borrow().read_u16(ptr), 0)
    }
    //....	X-indexed, indirect	 	OPC ($LL,X)	 	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    pub fn idx(&mut self) -> (u16, u8) {
        let ptr = self.bus.borrow().read_u8(self.pc).wrapping_add(self.x) as u16;
        self.pc += 1;
        (self.bus.borrow().read_u16(ptr), 0)
    }
    //....	indirect, Y-indexed	 	OPC ($LL),Y	 	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    pub fn idy(&mut self) -> (u16, u8) {
        let ptr = self.bus.borrow().read_u8(self.pc) as u16;
        self.pc += 1;

        let addr = self.bus.borrow().read_u16(ptr).wrapping_add(self.y as u16);
        let cycles = if ptr & 0xff00 == addr & 0xff00 { 0 } else { 1 };
        (addr, cycles)
    }
    //....	relative	 	        OPC $BB	 	    branch target is PC + signed offset BB ***
    pub fn rel(&mut self) -> (u16, u8) {
        let mut oper = self.bus.borrow().read_u8(self.pc) as u16;
        self.pc += 1;

        // If signed, set hi byte to FF
        if (oper & 0x80) != 0 {
            oper |= 0xff00;
        }

        (oper, 0)
    }
}
