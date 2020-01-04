mod address_modes;
mod instructions;
mod util;

pub use address_modes::AddressMode;
pub use instructions::{Instruction, Operation};

use std::cell::RefCell;
use std::rc::Rc;

use bit_field::BitField;

use super::bus::Bus;

#[derive(Copy, Clone, Eq, PartialEq)]
enum Interrupt {
    NMI = 0xfffa,
    RES = 0xfffc,
    IRQ = 0xfffe,
}

pub struct CPU {
    pub pc: u16, // program counter
    pub sp: u8,  // stack ptr
    pub x: u8,   // idx reg x
    pub y: u8,   // idx reg y
    pub a: u8,   // accumulator
    pub st: u8,  // status

    pub oper: u16, // Operand for the current instruction

    pub instruction: &'static Instruction,
    pub cycles: u8,

    interrupt: Option<Interrupt>,

    bus: Rc<RefCell<Bus>>,
}

impl CPU {
    // Flags
    pub const C: usize = 0; // Carry
    pub const Z: usize = 1; // Zero
    pub const I: usize = 2; // Interrupt
    pub const D: usize = 3; // Decimal
    pub const B: usize = 4; // Break
    pub const U: usize = 5; // Unused
    pub const V: usize = 6; // Overflow
    pub const N: usize = 7; // Negative

    pub fn get_flag(&self, f: usize) -> bool {
        self.st.get_bit(f)
    }

    pub fn set_flag(&mut self, f: usize, val: bool) {
        self.st.set_bit(f, val);
    }

    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        let pc = bus.borrow().cpu_read::<u16>(0xfffc);
        CPU {
            pc,
            x: 0,
            y: 0,
            a: 0,
            sp: 0xfd,
            st: 0x34,

            cycles: 0,

            oper: 0,

            interrupt: None,

            instruction: &Instruction::INVALID,

            bus,
        }
    }

    pub fn reset(&mut self) {
        self.interrupt = Some(Interrupt::RES);
    }

    pub fn nmi(&mut self) {
        self.interrupt = Some(Interrupt::NMI);
    }

    pub fn irq(&mut self) {
        if !self.get_flag(Self::I) {
            self.interrupt = Some(Interrupt::IRQ);
        }
    }

    pub fn clock(&mut self) {
        if self.cycles == 0 {
            match self.interrupt.take() {
                // Reset behaves differently so prioritise that
                Some(Interrupt::RES) => {
                    self.pc = self.bus.borrow().cpu_read(Interrupt::RES as u16);

                    self.a = 0;
                    self.x = 0;
                    self.y = 0;
                    self.sp = 0xfd;
                    self.st = 0x34;

                    self.oper = 0;
                    self.instruction = &Instruction::INVALID;

                    self.cycles = 8;
                }
                // IRQ and NMI behave the same
                Some(interrupt) => {
                    self.set_flag(Self::U, true);
                    self.set_flag(Self::B, false);

                    self.push_state();

                    self.set_flag(Self::I, true);

                    self.pc = self.bus.borrow().cpu_read::<u16>(interrupt as u16);

                    // NMI has one more cycle than IRQ
                    self.cycles = if interrupt == Interrupt::NMI { 8 } else { 7 };
                }
                _ => {
                    let code = self.bus.borrow().cpu_read::<u8>(self.pc);
                    self.instruction = &Self::INSTRUCTIONS[code as usize];
                    self.cycles = self.instruction.cycles;

                    if !self.instruction.is_valid() {
                        println!(
                            "Invalid instruction incoming at ${:04X} -- code: ${:02X}",
                            self.pc, code
                        );
                    }

                    self.pc += 1;

                    (self.instruction.addr_mode.method())(self);
                    (self.instruction.operation.method())(self);
                }
            }
        }

        self.cycles = self.cycles.saturating_sub(1);
    }

    /// invalid opcode found
    pub fn xxx(&mut self) {
        panic!("Unsupported instruction!")
    }

    /// Reads the operand according to address mode
    fn read_oper(&self) -> u8 {
        match self.instruction.addr_mode {
            AddressMode::ACC => self.a,
            AddressMode::IMM => self.oper as u8,
            _ => self.bus.borrow_mut().cpu_read(self.oper),
        }
    }

    /// Writes to the operand according to address mode
    fn write_oper(&mut self, v: u8) {
        match self.instruction.addr_mode {
            AddressMode::ACC => self.a = v,
            AddressMode::IMM => (),
            _ => self.bus.borrow_mut().cpu_write(self.oper, v),
        }
    }

    pub fn stack_addr(&self) -> u16 {
        0x100u16 + self.sp as u16
    }
}

use std::fmt::{Display, Formatter, Result};
impl Display for CPU {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        writeln!(fmt, "CPU")?;
        writeln!(fmt, "pc: ${:04X}", self.pc)?;
        writeln!(fmt, "sp: ${:04X}", self.sp)?;
        writeln!(fmt, "a:  {}", self.a)?;
        writeln!(fmt, "x:  {}", self.x)?;
        writeln!(fmt, "y:  {}", self.y)?;
        writeln!(fmt, "state:  NV-BDIZC\n        {:08b}", self.st)?;
        writeln!(fmt, "\ninstruction:")?;
        writeln!(fmt, "     code: ${:X}", self.instruction.code)?;
        writeln!(fmt, "       op: {:?}", self.instruction.operation)?;
        writeln!(fmt, "addr_mode: {:?}", self.instruction.addr_mode)?;

        match self.instruction.addr_mode {
            AddressMode::ACC => writeln!(fmt, "     oper:  {:2X}", self.a)?,
            AddressMode::IMM => writeln!(fmt, "     oper: #{:2X}", self.oper)?,
            _ => writeln!(fmt, "     oper: ${:04X}", self.oper)?,
        };

        writeln!(fmt, "   cycles: {}", self.cycles)?;
        Ok(())
    }
}
