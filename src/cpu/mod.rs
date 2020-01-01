mod address_modes;
mod helpers;
mod instructions;

pub use instructions::Instruction;

use std::cell::RefCell;
use std::rc::Rc;

use bit_field::BitField;

use address_modes::AddressMode;
use instructions::Operation;

use super::Bus;

#[derive(Debug)]
pub struct CPU {
    pc: u16, // program counter
    sp: u8,  // stack ptr
    x: u8,   // idx reg x
    y: u8,   // idx reg y
    a: u8,   // accumulator
    p: u8,   // status

    oper: u16, // Operation for the current instruction

    interrupt_addr: Option<u16>,
    instruction: Instruction,

    bus: Rc<RefCell<Bus>>,
}

impl CPU {
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
            sp: 0xFD,
            p: 0x34,
            oper: 0,
            interrupt_addr: None,
            instruction: Instruction::INVALID,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.interrupt_addr = Some(0xfffc);
    }
    pub fn nmi(&mut self) {
        self.set_flag(Self::B, false);
        self.push_state();
        self.interrupt_addr = Some(0xfffa);
    }

    pub fn irq(&mut self) {
        if !self.get_flag(Self::I) {
            self.set_flag(Self::B, false);
            self.push_state();
            self.interrupt_addr = Some(0xfffe);
        }
    }

    pub fn clock(&mut self) {
        if self.instruction.cycles == 0 {
            if let Some(addr) = self.interrupt_addr {
                self.set_flag(Self::I, true);

                self.pc = self.bus.borrow().read_u16(addr);
            }

            let code = self.bus.borrow().read_u8(self.pc);
            self.instruction = Self::INSTRUCTIONS[code as usize];

            if self.instruction.addr_mode == AddressMode::XXX
                || self.instruction.operation == Operation::XXX
            {
                println!("Invalid instruction incoming at {:X}", self.pc);
            }

            self.pc += 1;

            (self.instruction.addr_mode.method())(self);
            (self.instruction.operation.method())(self);

            println!("{}", self);
            print!("{}[2J", 27 as char);
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        self.instruction.cycles = self.instruction.cycles.saturating_sub(1);
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
            _ => self.bus.borrow_mut().read_u8(self.oper),
        }
    }

    /// Writes to the operand according to address mode
    fn write_oper(&mut self, v: u8) {
        match self.instruction.addr_mode {
            AddressMode::ACC => self.a = v,
            AddressMode::IMM => (),
            _ => self.bus.borrow_mut().write_u8(self.oper, v),
        }
    }

    fn stack_addr(&self) -> u16 {
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
        writeln!(fmt, "p:  NV-BDIZC\n    {:08b}", self.p)?;
        writeln!(fmt, "\ninstruction:")?;
        writeln!(fmt, "     code: ${:X}", self.instruction.code)?;
        writeln!(fmt, "       op: {:?}", self.instruction.operation)?;
        writeln!(fmt, "addr_mode: {:?}", self.instruction.addr_mode)?;

        match self.instruction.addr_mode {
            AddressMode::ACC => writeln!(fmt, "     oper:  {:2X}", self.a)?,
            AddressMode::IMM => writeln!(fmt, "     oper: #{:2X}", self.oper)?,
            _ => writeln!(fmt, "     oper: ${:04X}", self.oper)?,
        };

        writeln!(fmt, "   cycles: {}", self.instruction.cycles)?;
        Ok(())
    }
}
