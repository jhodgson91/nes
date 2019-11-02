mod address_modes;
mod helpers;
mod instructions;

pub use instructions::Instruction;

use std::cell::RefCell;
use std::rc::Rc;

use bit_field::BitField;

use address_modes::AddressMode;
use instructions::InstructionCode;

use super::Bus;

#[derive(Debug)]
pub struct CPU {
    pc: u16, // program counter
    s: u8,   // stack ptr
    x: u8,   // idx reg x
    y: u8,   // idx reg y
    a: u8,   // accumulator
    p: u8,   // status

    oper: u16, // Operation for the current instruction

    instruction: Instruction,

    bus: Rc<RefCell<Bus>>,
}

impl CPU {
    const STACK: u16 = 0x0100;

    // Flags
    const C: usize = 0; // Carry
    const Z: usize = 1; // Zero
    const I: usize = 2; // Interrupt
    const D: usize = 3; // Decimal
    const B: usize = 4; // Break
    const V: usize = 6; // Overflow
    const N: usize = 7; // Negative

    pub fn get_flag(&self, f: usize) -> bool {
        self.p.get_bit(f)
    }

    pub fn set_flag(&mut self, f: usize, val: bool) {
        self.p.set_bit(f, val);
    }

    pub fn new(bus: Rc<RefCell<Bus>>, pc: u16) -> Self {
        CPU {
            pc,
            x: 0,
            y: 0,
            a: 0,
            s: 0xFD,
            p: 0x34,
            oper: 0,
            instruction: Instruction::INVALID,
            bus,
        }
    }

    pub fn reset() {}
    pub fn nmi() {}
    pub fn irq() {}

    pub fn clock(&mut self) {
        if self.instruction.cycles == 0 {
            let op = self.bus.borrow().read_u8(self.pc);
            let (lower, upper) = ((op & 0x0f) as usize, (op >> 4) as usize);

            self.instruction = Self::INSTRUCTIONS[upper][lower];

            if self.instruction.addr_mode == AddressMode::XXX
                || self.instruction.code == InstructionCode::XXX
            {
                println!("Invalid instruction incoming at {:X}", self.pc);
            }

            self.pc += 1;

            (self.instruction.addr_mode.method())(self);
            (self.instruction.code.method())(self);
        }

        self.instruction.cycles = self.instruction.cycles.saturating_sub(1);
    }

    // invalid opcode found
    pub fn xxx(&mut self) {
        panic!("Unsupported instruction!")
    }
}
