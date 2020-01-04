use super::cartridge::Cartridge;
use num::PrimInt;

pub struct Bus {
    ram: [u8; 2 * 1024], // 2KB internal ram

    ppu_registers: [u8; 8], // 8-bytes of PPU registers
    io_registers: [u8; 32], // 20 bytes of IO registers

    cartridge: Cartridge,
}

trait Mapper {
    fn map(&self, addr: u16) -> (u16, &[u8]);
    fn map_mut(&mut self, addr: u16) -> (u16, &mut [u8]);
}

impl Mapper for Bus {
    fn map(&self, addr: u16) -> (u16, &[u8]) {
        match addr {
            0x000..=0x1fff => (addr & 0x07ff, &self.ram),
            0x2000..=0x3fff => (addr & 0x0007, &self.ppu_registers),
            0x4000..=0x401f => (addr - 0x4000, &self.io_registers),
            0x6000..=0x7fff => (addr - 0x6000, &self.cartridge.sram),
            _ => match self.cartridge.prg_size() {
                2 => (addr & 0x7fff, &self.cartridge.prg_rom),
                1 => (addr & 0x3fff, &self.cartridge.prg_rom),
                _ => panic!("Unsupported number of prg banks!"),
            },
        }
    }

    fn map_mut(&mut self, addr: u16) -> (u16, &mut [u8]) {
        match addr {
            0x000..=0x1fff => (addr & 0x07ff, &mut self.ram),
            0x2000..=0x3fff => (addr & 0x0007, &mut self.ppu_registers),
            0x4000..=0x401f => (addr - 0x4000, &mut self.io_registers),
            0x6000..=0x7fff => (addr - 0x6000, &mut self.cartridge.sram),
            _ => panic!(
                "Attempted to get write-access to cartridge at address ${:0X}",
                addr
            ),
        }
    }
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
        let (addr, mem) = self.map(addr);
        Self::read(addr, mem)
    }

    pub fn cpu_write<U: PrimInt>(&mut self, addr: u16, data: U) {
        let (addr, mem) = self.map_mut(addr);
        Self::write(addr, mem, data)
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
