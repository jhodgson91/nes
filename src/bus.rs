use super::cartridge::Cartridge;
use num::PrimInt;

pub struct Bus {
    ram: [u8; 2 * 1024], // 2KB internal ram

    ppu_registers: [u8; 8], // 8-bytes of PPU registers
    io_registers: [u8; 32], // 20 bytes of IO registers

    cartridge: Cartridge,
}

impl Bus {
    pub fn new(cartridge: Cartridge) -> Self {
        Bus {
            ram: [0; 2 * 1024],
            ppu_registers: [0; 8],
            io_registers: [0; 32],
            cartridge,
        }
    }

    pub fn cpu_read<U: PrimInt>(&self, addr: u16) -> U {
        match addr {
            0x000..=0x1fff => Self::read(addr & 0x07ff, &self.ram),
            0x2000..=0x3fff => Self::read(addr & 0x0007, &self.ppu_registers),
            0x4000..=0x401f => Self::read(addr - 0x4000, &self.io_registers),
            0x6000..=0x7fff => Self::read(addr - 0x6000, &self.cartridge.sram),
            _ => Self::read(self.cartridge.map(addr), &self.cartridge.prg_rom),
        }
    }

    pub fn cpu_write<U: PrimInt>(&mut self, addr: u16, data: U) {
        match addr {
            0x000..=0x1fff => Self::write(addr & 0x07ff, &mut self.ram, data),
            0x2000..=0x3fff => Self::write(addr & 0x0007, &mut self.ppu_registers, data),
            0x4000..=0x401f => Self::write(addr - 0x4000, &mut self.io_registers, data),
            0x6000..=0x7fff => Self::write(addr - 0x6000, &mut self.cartridge.sram, data),
            _ => panic!(
                "Attempted to get write-access to cartridge at address ${:0X}",
                addr
            ),
        }
    }

    fn read<U: PrimInt>(addr: u16, from: &[u8]) -> U {
        let addr = addr as usize;
        let p = &from[addr..addr + std::mem::size_of::<U>()];
        U::from_le(unsafe { std::ptr::read(p.as_ptr() as *const U) })
    }

    fn write<U: PrimInt>(addr: u16, to: &mut [u8], data: U) {
        let addr = addr as usize;
        let data = data.to_le();
        let sl = unsafe {
            // SAFETY - Known size of U guarantees we read and write only size_of<U> bytes
            std::slice::from_raw_parts(&data as *const U as *const u8, std::mem::size_of::<U>())
        };
        (&mut to[addr..addr + std::mem::size_of::<U>()]).copy_from_slice(&sl);
    }
}
