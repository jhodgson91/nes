use super::cartridge::Cartridge;

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

    pub fn cpu_read(&self, addr: u16) -> u8 {
        match addr {
            0x000..=0x1fff => self.ram[(addr & 0x07ff) as usize],
            0x2000..=0x3fff => self.ppu_registers[(addr & 0x0007) as usize],
            0x4000..=0x401f => self.io_registers[(addr - 0x4000) as usize],
            0x6000..=0x7fff => self.cartridge.sram[(addr - 0x6000) as usize],
            _ => self.cartridge.prg_rom[self.cartridge.cpu_map(addr) as usize],
        }
    }

    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x000..=0x1fff => self.ram[(addr & 0x07ff) as usize] = data,
            0x2000..=0x3fff => self.ppu_registers[(addr & 0x0007) as usize] = data,
            0x4000..=0x401f => self.io_registers[(addr - 0x4000) as usize] = data,
            0x6000..=0x7fff => self.cartridge.sram[(addr - 0x6000) as usize] = data,
            _ => panic!(
                "Attempted to get write-access to cartridge at address ${:04X}",
                addr
            ),
        }
    }

    pub fn ppu_read(&self, mut addr: u16) -> u8 {
        // Mirror entire address space
        addr &= 0x3fff;

        match addr {
            // TODO - cartridge should have more control over this via mappers
            0x0000..=0x1fff => self.cartridge.chr_rom[addr as usize],
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
                self.nametables[(addr >> shift) as usize][(addr & 0x03ff) as usize]
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
                self.palettes[addr as usize]
            }
            _ => panic!("Unknown ppu address! {:04X}", addr),
        }
    }

    pub fn ppu_write(&mut self, mut addr: u16, data: u8) {
        match addr {
            // TODO - cartridge should have more control over this via mappers
            0x0000..=0x1fff => self.cartridge.chr_rom[addr as usize] = data,
            0x2000..=0x3eff => {
                addr &= 0x0fff;

                // See read for explanation of this!
                let shift = 11 - self.cartridge.header.mirror_mode() as u8;
                self.nametables[(addr >> shift) as usize][(addr & 0x03ff) as usize] = data
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
                self.palettes[addr as usize] = data
            }
            _ => panic!("Unknown ppu address! {:04X}", addr),
        }
    }
}
