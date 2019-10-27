mod cpu;

use cpu::CPU;

use std::cell::RefCell;
use std::rc::Rc;

pub struct Bus {
    memory: [u8; 64 * 1024],
}

impl Bus {
    pub fn read_u8(&self, addr: u16) -> u8 {
        self.memory[Self::map_addr(addr) as usize]
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let mapped = Self::map_addr(addr) as usize;
        let mut bytes = [0u8; 2];
        bytes.copy_from_slice(&self.memory[mapped..=mapped + 1]);
        u16::from_le_bytes(bytes)
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        self.memory[Self::map_addr(addr) as usize] = data;
    }

    pub fn write_range(&mut self, addr: u16, data: &[u8]) {
        let addr = addr as usize;
        (&mut self.memory[addr..data.len()]).copy_from_slice(data);
    }

    fn map_addr(addr: u16) -> u16 {
        const IO_END: u16 = 0x3fff;
        const RAM_END: u16 = 0x1fff;
        const RAM_MIRROR_START: u16 = 0x1ff;

        match addr {
            0..=RAM_END => addr & RAM_MIRROR_START, // Handle RAM mirroring
            RAM_END..=IO_END => (addr & 0x7) | 0x2000, // Handle IO register mirroring
            _ => addr,
        }
    }
}

struct NES {
    cpu: CPU,
}

impl Default for NES {
    fn default() -> Self {
        let bus = Rc::new(RefCell::new(Bus {
            memory: [0; 64 * 1024],
        }));

        NES {
            cpu: CPU::new(bus.clone(), 0),
        }
    }
}

macro_rules! instruction {
    ($ins:ident, $addr: ident, $cycles:expr) => {
        Instruction {
            name: stringify!($ins),
            op: paste::expr! { &CPU::$ins },
            addr_mode: paste::expr! { &CPU::$addr },
            cycles: $cycles,
        }
    };
}

fn main() {
    let mut n = NES::default();
}
