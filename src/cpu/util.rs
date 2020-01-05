use super::AddressMode;
use super::{Flags, CPU};

impl CPU {
    pub fn disassemble(&self, mut pc: u32, to: u16) -> Vec<(u16, String)> {
        let bus = self.bus.borrow();
        let mut res = Vec::new();

        while pc < to as u32 {
            let line_addr = pc as u16;

            let mut s = format!("${:04X}: ", pc);
            let instruction = Self::INSTRUCTIONS[bus.cpu_read::<u8>(pc as u16) as usize];
            pc += 1;

            if ((to - instruction.addr_mode.operand_size() as u16) as u32) < pc {
                break;
            }

            s += &format!("{:?} ", instruction.operation);
            match instruction.addr_mode {
                AddressMode::ACC => (),
                AddressMode::AB0 => {
                    s += &format!("${:04X} ", bus.cpu_read::<u16>(pc as u16));
                    pc += 2;
                }
                AddressMode::ABX => {
                    s += &format!("${:04X},X", bus.cpu_read::<u16>(pc as u16));
                    pc += 2;
                }
                AddressMode::ABY => {
                    s += &format!("${:04X},Y", bus.cpu_read::<u16>(pc as u16));
                    pc += 2;
                }
                AddressMode::IMM => {
                    s += &format!("#${:02X}", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::IMP => (),
                AddressMode::ID0 => {
                    s += &format!("(${:04X})", bus.cpu_read::<u16>(pc as u16));
                    pc += 2;
                }
                AddressMode::IDX => {
                    s += &format!("(${:02X},X)", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::IDY => {
                    s += &format!("(${:02X}),Y", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::REL => {
                    let oper = bus.cpu_read::<u8>(pc as u16) as u16;
                    let dest = (pc as u16).wrapping_add(if oper & 1 << 7 != 0 {
                        oper | 0xff00
                    } else {
                        oper
                    });

                    s += &format!("${:02X} - [${:04X}]", oper, dest);

                    pc += 1;
                }
                AddressMode::ZP0 => {
                    s += &format!("${:02X}", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::ZPX => {
                    s += &format!("${:02X},X", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::ZPY => {
                    s += &format!("${:02X},Y", bus.cpu_read::<u8>(pc as u16));
                    pc += 1;
                }
                AddressMode::XXX => (),
            };

            s += &format!(" {{{:?}}}", instruction.addr_mode);

            res.push((line_addr, s));
        }

        res
    }

    pub(super) fn branch_on_condition(&mut self, cond: bool, addr: u16) {
        if cond {
            self.cycles += 1;

            let addr = self.pc.wrapping_add(addr);
            if addr & 0xff00 != self.pc & 0xff00 {
                self.cycles += 1;
            }

            self.pc = addr;
        }
    }

    pub(super) fn compare(&mut self, lhs: u8, rhs: u8) {
        self.set_flag(Flags::C, lhs >= rhs);
        self.set_flag(Flags::Z, lhs == rhs);
        self.set_flag(Flags::N, (lhs.wrapping_sub(rhs)) & 1 << 7 != 0);
    }

    pub(super) fn push_state(&mut self) {
        // PC should be written to the sp and one before, so dec first, then write u16, then dec
        self.sp = self.sp.wrapping_sub(1);
        self.bus.borrow_mut().cpu_write(self.stack_addr(), self.pc);
        self.sp = self.sp.wrapping_sub(1);

        self.bus
            .borrow_mut()
            .cpu_write(self.stack_addr(), self.st.bits());
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(super) fn pop_state(&mut self) {
        self.sp = self.sp.wrapping_add(1);
        self.st = Flags::from_bits(self.bus.borrow_mut().cpu_read(self.stack_addr())).unwrap();

        self.sp = self.sp.wrapping_add(1);
        self.pc = self.bus.borrow().cpu_read(self.stack_addr());
        self.sp = self.sp.wrapping_add(1);
    }
}
