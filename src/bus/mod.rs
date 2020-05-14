mod ppu_state;
use ppu_state::*;

use super::Cartridge;

pub trait Mapper {
    fn map_cpu_read(&self, addr: u16) -> u16 {
        addr
    }
    fn map_cpu_write(&mut self, addr: u16) -> u16 {
        addr
    }

    fn map_ppu_read(&self, addr: u16) -> u16 {
        addr
    }
    fn map_ppu_write(&mut self, addr: u16) -> u16 {
        addr
    }
}

struct IdentityMapper;
impl Mapper for IdentityMapper {}

struct Mapper000 {
    mirroring: u8,
    prg_banks: u8,
}

impl Mapper000 {
    fn map_cpu(&self, addr: u16) -> u16 {
        match addr {
            0x8000..=0xffff => match self.prg_banks {
                2 => addr & 0x7fff,
                1 => addr & 0x3fff,
                _ => panic!("Mapper000 does not support {} PRG banks", self.prg_banks),
            },
            _ => addr,
        }
    }

    fn map_ppu(&self, addr: u16) -> u16 {
        match addr {
            0x2000..=0x3eff => {
                let shift = 11 - self.mirroring as u8;
                let idx = (addr >> shift) & 1;
                (addr & 0x3ff) + idx * 0x400
            }
            _ => addr,
        }
    }
}
impl Mapper for Mapper000 {
    fn map_cpu_read(&self, addr: u16) -> u16 {
        self.map_cpu(addr)
    }

    fn map_cpu_write(&mut self, addr: u16) -> u16 {
        self.map_cpu(addr)
    }

    fn map_ppu_read(&self, addr: u16) -> u16 {
        self.map_ppu(addr)
    }

    fn map_ppu_write(&mut self, addr: u16) -> u16 {
        self.map_ppu(addr)
    }
}

pub trait NesPrimitive {
    fn from_u8(v: u8) -> Self;
    fn into_u8(self) -> u8;

    fn read(addr: u16, from: &[u8]) -> Self;
    fn write(self, addr: u16, to: &mut [u8]);
}

impl NesPrimitive for u8 {
    fn read(addr: u16, from: &[u8]) -> Self {
        from[addr as usize]
    }
    fn write(self, addr: u16, to: &mut [u8]) {
        to[addr as usize] = self
    }
    fn from_u8(v: u8) -> Self {
        v
    }
    fn into_u8(self) -> u8 {
        self
    }
}

impl NesPrimitive for u16 {
    fn read(addr: u16, from: &[u8]) -> Self {
        let lo = from[addr as usize] as u16;
        let hi = (from[addr.wrapping_add(1) as usize] as u16) << 8;
        hi | lo
    }
    fn write(self, addr: u16, to: &mut [u8]) {
        to[addr as usize] = self as u8;
        to[addr.wrapping_add(1) as usize] = (self >> 8) as u8;
    }
    fn from_u8(v: u8) -> Self {
        v as u16
    }
    fn into_u8(self) -> u8 {
        self as u8
    }
}

pub struct Bus {
    // CPU data connected to bus
    ram: [u8; 0x800],
    ppu_state: PPUState,
    io_registers: [u8; 0x20],
    sram: Vec<u8>,
    prg_rom: Vec<u8>,

    // PPU data connected to bus
    pattern_tables: Vec<u8>,
    nametables: Vec<u8>,
    palettes: [u8; 0x20],

    // Mapper from cartridge
    mapper: Box<dyn Mapper>,
}

impl Default for Bus {
    fn default() -> Self {
        Bus {
            ram: [0; 0x800],
            ppu_state: PPUState::default(),
            io_registers: [0; 0x20],
            sram: vec![0; 0x3fe0],
            prg_rom: Vec::new(),

            pattern_tables: vec![0; 0x2000],
            nametables: vec![0; 0x8000],
            palettes: [0; 0x20],

            mapper: Box::new(IdentityMapper),
        }
    }
}

impl Bus {
    pub fn from_cartridge(cartridge: Cartridge) -> Self {
        let mut bus = Self::default();

        match cartridge.header.mapper_number() {
            0 => {
                bus.mapper = Box::new(Mapper000 {
                    prg_banks: cartridge.prg_size() as u8,
                    mirroring: cartridge.header.mirror_mode(),
                })
            }
            _ => (),
        };

        bus.prg_rom = cartridge.prg_rom;
        bus.pattern_tables = cartridge.chr_rom;

        bus
    }

    pub fn cpu_read<U: NesPrimitive>(&mut self, addr: u16) -> U {
        match addr {
            0x0000..=0x1fff => U::read(addr & 0x7ff, &mut self.ram),
            0x2000..=0x3fff => U::from_u8(self.ppu_state_read(addr)),
            0x4000..=0x401f => U::read(addr & 0x1f, &mut self.io_registers),
            0x4020..=0x7fff => U::read(addr - 0x4020, &mut self.sram),
            0x8000..=0xffff => U::read(self.mapper.map_cpu_read(addr), &mut self.prg_rom),
        }
    }

    pub fn cpu_write<U: NesPrimitive>(&mut self, addr: u16, v: U) {
        match addr {
            0x0000..=0x1fff => U::write(v, addr & 0x7ff, &mut self.ram),
            0x2000..=0x3fff => self.ppu_state_write(addr, v.into_u8()),
            0x4000..=0x401f => U::write(v, addr & 0x1f, &mut self.io_registers),
            0x4020..=0x7fff => U::write(v, addr - 0x4020, &mut self.sram),
            0x8000..=0xffff => U::write(v, self.mapper.map_cpu_write(addr), &mut self.prg_rom),
        }
    }

    pub fn ppu_read<U: NesPrimitive>(&mut self, mut addr: u16) -> U {
        addr &= 0x3fff;
        match addr {
            0x0000..=0x1fff => U::read(addr, &mut self.pattern_tables),
            0x2000..=0x3eff => U::read(self.mapper.map_ppu_read(addr), &mut self.nametables),
            0x3f00..=0x3fff => U::read(Self::map_palette_addr(addr), &mut self.palettes),
            0x4000..=0xffff => panic!("Address {} out of PPU range!"),
        }
    }

    pub fn ppu_write<U: NesPrimitive>(&mut self, mut addr: u16, v: U) {
        addr &= 0x3fff;

        match addr {
            0x0000..=0x1fff => U::write(v, addr, &mut self.pattern_tables),
            0x2000..=0x3eff => U::write(v, self.mapper.map_ppu_write(addr), &mut self.nametables),
            0x3f00..=0x3fff => U::write(v, Self::map_palette_addr(addr), &mut self.palettes),
            0x4000..=0xffff => (),
        }
    }

    fn map_palette_addr(mut addr: u16) -> u16 {
        addr &= 0x1f;
        match addr {
            0x10 => 0x00,
            0x14 => 0x04,
            0x18 => 0x08,
            0x1c => 0x0c,
            _ => addr,
        }
    }

    fn ppu_state_read(&mut self, addr: u16) -> u8 {
        let mut rv = 0;
        match addr & 0x7 {
            0x2 => {
                let s = &mut self.ppu_state;
                s.status.set_vblank(false);
                s.addr_latch = false;
                rv = (s.status.0 & 0xe0) | s.data_buffer & 0x1f;
            }
            0x7 => {
                rv = self.ppu_state.data_buffer;
                self.ppu_state.data_buffer = self.ppu_read(self.ppu_state.vram_addr.0);

                if self.ppu_state.vram_addr.0 >= 0x3f00 {
                    rv = self.ppu_state.data_buffer;
                }

                self.ppu_state.vram_addr.0 += if self.ppu_state.ctrl.increment_mode() {
                    32
                } else {
                    1
                };
            }
            _ => (),
        }
        rv
    }

    fn ppu_state_write(&mut self, addr: u16, v: u8) {
        match addr & 0x7 {
            0x0 => {
                let s = &mut self.ppu_state;

                s.ctrl.0 = v;
                s.vram_addr.set_nametable_x(s.ctrl.nametable_x());
                s.vram_addr.set_nametable_y(s.ctrl.nametable_y());
            }
            0x1 => self.ppu_state.mask.0 = v,
            0x5 => {
                let s = &mut self.ppu_state;

                // SCROLL
                if s.addr_latch {
                    s.fine_x = v & 0x7;
                    s.tram_addr.set_coarse_x(v >> 3);
                } else {
                    s.tram_addr.set_fine_y(v & 0x7);
                    s.tram_addr.set_coarse_y(v >> 3);
                }

                self.ppu_state.addr_latch = !self.ppu_state.addr_latch;
            }
            0x6 => {
                let s = &mut self.ppu_state;

                // ADDR
                if s.addr_latch {
                    // First write sets hi byte of internal temp addr without affecting lo!
                    // 0x3f is first 6 bytes, since technically PPU address space is 14-bit
                    s.tram_addr.0 = {
                        let v = (v & 0x3f) as u16;
                        v << 8 | s.tram_addr.0 & 0x00ff
                    }
                } else {
                    // Second write sets lo byte, and copies to vram_addr
                    s.tram_addr.0 = (s.tram_addr.0 & 0xff00) | v as u16;
                    s.vram_addr = s.tram_addr;
                }

                self.ppu_state.addr_latch = !self.ppu_state.addr_latch;
            }
            0x7 => {
                self.ppu_write(self.ppu_state.vram_addr.0, v);
                self.ppu_state.vram_addr.0 += if self.ppu_state.ctrl.increment_mode() {
                    32
                } else {
                    1
                };
            }
            _ => (),
        }
    }
}

mod tests {

    #[test]
    fn ram_mirroring() {
        let mut bus = super::Bus::default();
        for i in 0..=0x7ff {
            bus.cpu_write(i, std::u8::MAX);
            assert_eq!(bus.cpu_read::<u8>(i), std::u8::MAX);
            assert_eq!(bus.cpu_read::<u8>(i + 0x800), std::u8::MAX);
            assert_eq!(bus.cpu_read::<u8>(i + 0x1000), std::u8::MAX);
            assert_eq!(bus.cpu_read::<u8>(i + 0x1800), std::u8::MAX);
        }

        let mut bus = super::Bus::default();
        for i in (0..=0x7ff).step_by(2) {
            bus.cpu_write(i, std::u16::MAX);
            assert_eq!(bus.cpu_read::<u16>(i), std::u16::MAX);
            assert_eq!(bus.cpu_read::<u16>(i + 0x800), std::u16::MAX);
            assert_eq!(bus.cpu_read::<u16>(i + 0x1000), std::u16::MAX);
            assert_eq!(bus.cpu_read::<u16>(i + 0x1800), std::u16::MAX);
        }
    }

    #[test]
    fn palette_mirroring() {
        let mut bus = super::Bus::default();

        let specials = &[0x3f10, 0x3f14, 0x3f18, 0x3f1c];

        for i in 1..7 {
            let base = 0x3f00 + i * 0x20;

            for j in 0..=0x1f {
                bus.ppu_write(base + j, j as u8);
                assert_eq!(bus.ppu_read::<u8>(0x3f00 + j), j as u8);
                if specials.contains(&(0x3f00 + j)) {
                    assert_eq!(bus.ppu_read::<u8>(0x3f00 + (j - 0x10)), j as u8);
                }
            }
        }
    }
}
