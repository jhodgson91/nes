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
        self.set_flag(Self::N, (lhs - rhs).get_bit(7));
    }
}
