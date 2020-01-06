use super::cartridge::Cartridge;
use super::ppu::*;
use num::PrimInt;

pub struct Bus {
    // CPU data
    ram: [u8; 2 * 1024],    // 2KB internal ram
    ppu_registers: [u8; 8], // 8-bytes of PPU registers
    io_registers: [u8; 32], // 20 bytes of IO registers

    palettes: [u8; 32],
    nametables: [[u8; 1024]; 2],

    cartridge: Cartridge,
}

impl Bus {
    pub fn new(cartridge: Cartridge) -> Self {
        Bus {
            // CPU
            ram: [0; 2 * 1024],
            ppu_registers: [0; 8],
            io_registers: [0; 32],

            palettes: [0; 32],
            nametables: [[0; 1024]; 2],

            cartridge,
        }
    }

    pub fn cpu_read<U: PrimInt>(&self, addr: u16) -> U {
        match addr {
            0x000..=0x1fff => Self::read(addr & 0x07ff, &self.ram),
            0x2000..=0x3fff => Self::read(addr & 0x0007, &self.ppu_registers),
            0x4000..=0x401f => Self::read(addr - 0x4000, &self.io_registers),
            0x6000..=0x7fff => Self::read(addr - 0x6000, &self.cartridge.sram),
            _ => Self::read(self.cartridge.cpu_map(addr), &self.cartridge.prg_rom),
        }
    }

    pub fn cpu_write<U: PrimInt>(&mut self, addr: u16, data: U) {
        match addr {
            0x000..=0x1fff => Self::write(addr & 0x07ff, &mut self.ram, data),
            0x2000..=0x3fff => Self::write(addr & 0x0007, &mut self.ppu_registers, data),
            0x4000..=0x401f => Self::write(addr - 0x4000, &mut self.io_registers, data),
            0x6000..=0x7fff => Self::write(addr - 0x6000, &mut self.cartridge.sram, data),
            _ => panic!(
                "Attempted to get write-access to cartridge at address ${:04X}",
                addr
            ),
        }
    }

    pub fn ppu_read<U: PrimInt>(&self, mut addr: u16) -> U {
        // Mirror entire address space
        addr &= 0x3fff;

        match addr {
            // TODO - cartridge should have more control over this via mappers
            0x0000..=0x1fff => Self::read(addr, &self.cartridge.chr_rom),
            0x2000..=0x3eff => {
                addr &= 0x0fff;

                // bits 11 and 10 of the address can tell us the nametable to address into
                // We have a 12-bit address here ( top byte is always 0 thanks to mirroring )
                // The right 10-bits address the 1KB table
                // The top 2-bits can be used to determine which table to address

                // This table describes the address ranges, the top two bits
                // and the table to use in each mirror mode
                //     RANGE      | top 2 bits | nametable idx
                // 0x000 -> 0x3ff |     00     |  H = 0, V = 0
                // 0x400 -> 0x7ff |     01     |  H = 0, V = 1
                // 0x800 -> 0xbff |     10     |  H = 1, V = 0
                // 0xc00 -> 0xfff |     11     |  H = 1, V = 1

                // The 11th bit matches horizontal mirror indexing, and the 10th matches vertical
                // so shifting by the correct amount is enough! We can decide by how much we shift
                // by treating the cartridge mirroring as a u8
                // if horizontal, this is 0, so shift by 11
                // if vertical, this is 1, so shift by 10
                let shift = 11 - self.cartridge.header.mirror_mode() as u8;
                Self::read(addr & 0x03ff, &self.nametables[(addr >> shift) as usize])
            }
            0x3f00..=0x3fff => {
                addr &= 0x1f;
                match addr {
                    0x10 => addr = 0x00,
                    0x14 => addr = 0x04,
                    0x18 => addr = 0x08,
                    0x1c => addr = 0x0c,
                    _ => (),
                };
                Self::read(addr, &self.palettes)
            }
            _ => panic!("Unknown ppu address! {:04X}", addr),
        }
    }

    pub fn ppu_write<U: PrimInt>(&mut self, mut addr: u16, data: U) {
        match addr {
            // TODO - cartridge should have more control over this via mappers
            0x0000..=0x1fff => Self::write(addr, &mut self.cartridge.chr_rom, data),
            0x2000..=0x3eff => {
                addr &= 0x0fff;

                // See read for explanation of this!
                let shift = 11 - self.cartridge.header.mirror_mode() as u8;
                Self::write(
                    addr & 0x03ff,
                    &mut self.nametables[(addr >> shift) as usize],
                    data,
                )
            }
            0x3f00..=0x3fff => {
                addr &= 0x1f;
                match addr {
                    0x10 => addr = 0x00,
                    0x14 => addr = 0x04,
                    0x18 => addr = 0x08,
                    0x1c => addr = 0x0c,
                    _ => (),
                };
                Self::write(addr, &mut self.palettes, data)
            }
            _ => panic!("Unknown ppu address! {:04X}", addr),
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
