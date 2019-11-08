use super::CPU;

use bit_field::BitField;

impl CPU {
    pub(super) fn branch_on_condition(&mut self, cond: bool, addr: u16) {
        if cond {
            self.instruction.cycles += 1;

            let addr = self.pc.wrapping_add(addr);
            if addr & 0xff00 != self.pc & 0xff00 {
                self.instruction.cycles += 1;
            }

            self.pc = addr;
        }
    }

    pub(super) fn compare(&mut self, lhs: u8, rhs: u8) {
        self.set_flag(Self::C, lhs >= rhs);
        self.set_flag(Self::Z, lhs == rhs);
        self.set_flag(Self::N, (lhs.wrapping_sub(rhs)).get_bit(7));
    }

    pub(super) fn store_state(&mut self) {
        // Write program counter to stack and dec the stack pointer
        self.bus.borrow_mut().write_u16(self.stack_addr(), self.pc);
        self.sp = self.sp.wrapping_sub(2);

        // write status to stack and dec stack pointer
        self.bus.borrow_mut().write_u8(self.stack_addr(), self.p);
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(super) fn restore_state(&mut self) {
        self.sp = self.sp.wrapping_add(1);
        self.p = self.bus.borrow_mut().read_u8(self.stack_addr());

        self.sp = self.sp.wrapping_add(2);
        self.pc = self.bus.borrow().read_u16(self.stack_addr());
    }
}
