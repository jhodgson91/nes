mod address_modes;
mod instructions;

pub use instructions::Instruction;

use std::cell::RefCell;
use std::rc::Rc;

use super::Bus;

pub struct CPU {
    pc: u16, // program counter
    s: u8,   // stack ptr
    x: u8,   // idx reg x
    y: u8,   // idx reg y
    a: u8,   // accumulator
    p: u8,   // status

    instruction: Instruction,

    bus: Rc<RefCell<Bus>>,
}

impl CPU {
    const INSTRUCTIONS: [[Instruction; 16]; 16] = [[Instruction::INVALID; 16]; 16];

    pub fn new(bus: Rc<RefCell<Bus>>, pc: u16) -> Self {
        CPU {
            pc,
            s: 0,
            x: 0,
            y: 0,
            a: 0,
            p: 0,
            instruction: Instruction::INVALID,
            bus,
        }
    }

    pub fn reset() {}
    pub fn nmi() {}
    pub fn irq() {}

    fn clock(&mut self) {
        if self.instruction.cycles == 0 {
            let op = self.bus.borrow().read_u8(self.pc);
            self.pc += 1;
            let (lower, upper) = ((op & 0x0f) as usize, (op >> 4) as usize);

            self.instruction = Self::INSTRUCTIONS[upper][lower];
            let (addr, mut extra_cycles) = (self.instruction.addr_mode)(self);
            extra_cycles += (self.instruction.op)(self, addr);
            self.instruction.cycles += extra_cycles;
        }

        self.instruction.cycles -= 1;
    }
}
