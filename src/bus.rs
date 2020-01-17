use super::cartridge::Cartridge;

type AddressRange = std::ops::RangeInclusive<u16>;

pub trait NesPrimitive {
    fn read(addr: u16, from: &dyn Addressable) -> Self;
    fn write(self, addr: u16, to: &mut dyn Addressable);
}

impl NesPrimitive for u8 {
    fn read(addr: u16, from: &dyn Addressable) -> Self {
        from.read(addr)
    }
    fn write(self, addr: u16, to: &mut dyn Addressable) {
        to.write(addr, self)
    }
}

impl NesPrimitive for u16 {
    fn read(addr: u16, from: &dyn Addressable) -> Self {
        from.read_u16(addr)
    }
    fn write(self, addr: u16, to: &mut dyn Addressable) {
        to.write_u16(addr, self)
    }
}

pub trait Addressable {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, v: u8);

    fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi = self.read(addr.wrapping_add(1)) as u16;
        lo | (hi << 8)
    }

    fn write_u16(&mut self, addr: u16, v: u16) {
        self.write(addr, v as u8);
        self.write(addr.wrapping_add(1), (v >> 8) as u8)
    }
}

struct PPU {
    registers: [u8; 8],
}
impl Addressable for PPU {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x2000..=0x2007 => self.registers[(addr & 0x7) as usize],
            _ => panic!("Invalid PPU address"),
        }
    }
    fn write(&mut self, addr: u16, v: u8) {
        match addr {
            0x2000..=0x2007 => self.registers[(addr & 0x7) as usize] = v,
            _ => panic!("Invalid PPU address"),
        }
    }
}

struct Palettes([u8; 0x20]);
impl Palettes {
    fn new() -> Self {
        Palettes([0; 0x20])
    }

    fn map_addr(&self, mut addr: u16) -> u16 {
        addr &= 0x1f;
        match addr {
            0x10 => 0x00,
            0x14 => 0x04,
            0x18 => 0x08,
            0x1c => 0x0c,
            _ => addr,
        }
    }
}

impl Addressable for Palettes {
    fn read(&self, addr: u16) -> u8 {
        self.0[self.map_addr(addr) as usize]
    }
    fn write(&mut self, addr: u16, v: u8) {
        self.0[self.map_addr(addr) as usize] = v
    }
}

struct AddressSpace {
    device: Box<dyn Addressable>,
    range: AddressRange, // range the device will respond to

    sub_range: Option<AddressRange>, // Sub-range of valid addresses
}

impl AddressSpace {
    fn new(range: AddressRange, device: impl Addressable + 'static) -> Self {
        AddressSpace {
            range,
            sub_range: None,
            device: Box::new(device),
        }
    }

    fn mirrored(
        addressable: AddressRange,
        sub_range: AddressRange,
        device: impl Addressable + 'static,
    ) -> Self {
        AddressSpace {
            range: addressable,
            sub_range: Some(sub_range),
            device: Box::new(device),
        }
    }

    fn map_addr(&self, addr: u16) -> u16 {
        match &self.sub_range {
            Some(sub) => addr % (sub.end() - sub.start() + 1),
            None => addr % (self.range.end() - self.range.start() + 1),
        }
    }
}

impl Addressable for AddressSpace {
    fn read(&self, addr: u16) -> u8 {
        self.device.read(self.map_addr(addr))
    }
    fn write(&mut self, addr: u16, v: u8) {
        self.device.write(self.map_addr(addr), v)
    }
}

impl Addressable for Vec<u8> {
    fn read(&self, addr: u16) -> u8 {
        self[addr as usize]
    }
    fn write(&mut self, addr: u16, v: u8) {
        self[addr as usize] = v
    }
}

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

pub struct Bus {
    cpu: Vec<AddressSpace>,
    ppu: Vec<AddressSpace>,

    mapper: Box<dyn Mapper>,
}

impl Default for Bus {
    fn default() -> Self {
        Bus {
            cpu: Vec::new(),
            ppu: Vec::new(),
            mapper: Box::new(IdentityMapper),
        }
    }
}

impl Bus {
    pub fn from_cartridge(cartridge: Cartridge) -> Self {
        let mut bus = Self::default();

        match cartridge.header.mapper_number() {
            0 => bus.set_mapper(Mapper000 {
                prg_banks: cartridge.prg_size() as u8,
                mirroring: cartridge.header.mirror_mode(),
            }),
            _ => (),
        };

        bus.cpu_connect(AddressSpace::mirrored(
            0..=0x1fff,
            0..=0x7ff,
            vec![0; 0x800],
        ));
        bus.cpu_connect(AddressSpace::mirrored(
            0x2000..=0x3fff,
            0x2000..=0x2007,
            vec![0; 8],
        ));
        bus.cpu_connect(AddressSpace::new(0x4000..=0x401f, vec![0; 32]));
        bus.cpu_connect(AddressSpace::new(0x4020..=0x7fff, vec![0; 0x3FE0]));
        bus.cpu_connect(AddressSpace::new(0x8000..=0xffff, cartridge.prg_rom));

        bus.ppu_connect(AddressSpace::new(0x0..=0x1fff, cartridge.chr_rom));
        bus.ppu_connect(AddressSpace::mirrored(
            0x2000..=0x3eff,
            0x2000..=0x2fff,
            vec![0; 0x8000],
        ));
        bus.ppu_connect(AddressSpace::mirrored(
            0x3f00..=0x3fff,
            0x3f00..=0x3f1f,
            Palettes::new(),
        ));
    
        bus
    }

    pub fn new(mapper: impl Mapper + 'static) -> Self {
        Bus {
            cpu: Vec::new(),
            ppu: Vec::new(),
            mapper: Box::new(mapper),
        }
    }

    fn set_mapper(&mut self, mapper: impl Mapper + 'static) {
        self.mapper = Box::new(mapper);
    }

    pub fn cpu_read<P: NesPrimitive>(&self, addr: u16) -> P {
        let device = self
            .cpu
            .iter()
            .find(|d| d.range.contains(&addr))
            .expect(&format!(
                "addr {:04X} has not been mapped on CPU bus!",
                addr
            ));
        P::read(self.mapper.map_cpu_read(addr), device)
    }

    pub fn cpu_write<P: NesPrimitive>(&mut self, addr: u16, v: P) {
        let device = self
            .cpu
            .iter_mut()
            .find(|d| d.range.contains(&addr))
            .expect(&format!(
                "addr {:04X} has not been mapped on CPU bus!",
                addr
            ));
        v.write(self.mapper.map_cpu_write(addr), device)
    }

    fn cpu_connect(&mut self, device: AddressSpace) {
        self.cpu.push(device);
    }

    pub fn ppu_read<P: NesPrimitive>(&self, addr: u16) -> P {
        let device = self
            .ppu
            .iter()
            .find(|d| d.range.contains(&addr))
            .expect(&format!(
                "addr {:04X} has not been mapped on PPU bus!",
                addr
            ));
        P::read(self.mapper.map_ppu_read(addr), device)
    }

    pub fn ppu_write<P: NesPrimitive>(&mut self, addr: u16, v: P) {
        let device = self
            .ppu
            .iter_mut()
            .find(|d| d.range.contains(&addr))
            .expect(&format!(
                "addr {:04X} has not been mapped on PPU bus!",
                addr
            ));
        v.write(self.mapper.map_ppu_write(addr), device)
    }

    fn ppu_connect(&mut self, device: AddressSpace) {
        self.ppu.push(device);
    }
}