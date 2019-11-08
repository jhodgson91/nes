mod cpu;

use cpu::CPU;

use std::cell::RefCell;
use std::rc::Rc;

use std::convert::AsRef;
use std::fmt::Debug;
use std::fs::File;
use std::io::{Error, ErrorKind, Read};
use std::path::Path;

pub struct Bus {
    memory: [u8; 64 * 1024],
}

impl Debug for Bus {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Bus {
    pub fn read_u8(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let addr = addr as usize;
        let mut bytes = [0u8; 2];
        bytes.copy_from_slice(&self.memory[addr..=addr + 1]);
        u16::from_le_bytes(bytes)
    }

    pub fn write_u16(&mut self, addr: u16, data: u16) {
        let addr = addr as usize;
        (&mut self.memory[addr..=addr + 1]).copy_from_slice(&data.to_le_bytes());
    }

    pub fn write_range(&mut self, addr: u16, data: &[u8]) {
        let addr = addr as usize;
        (&mut self.memory[addr..addr + data.len()]).copy_from_slice(data);
    }
}

struct ROM {
    header: [u8; 16],

    prg_rom: Vec<u8>,
    trainer: Option<[u8; 512]>,
    chrom: Option<Vec<u8>>,
    misc: Option<Vec<u8>>,

    bus: Rc<RefCell<Bus>>,
}

impl ROM {
    pub fn from_file(bus: Rc<RefCell<Bus>>, mut file: File) -> std::io::Result<Self> {
        let mut header = [0u8; 16];
        file.read_exact(&mut header).unwrap();

        // trainer
        let trainer = if header[6] & 0x40 != 0 {
            let mut data = [0u8; 512];
            file.read_exact(&mut data)?;
            Some(data)
        } else {
            None
        };

        let prg_rom = {
            // first 4 bits of byte 9 == msb
            let msb = (header[9] & 0xf) as u16;
            // byte 4 == lsb
            let lsb = header[4] as u16;

            let size = Self::parse_size(lsb | msb);
            if size > 2 || size == 0 {
                return Err(Error::new(
                    ErrorKind::InvalidInput,
                    format!("prg_rom size must be either 1 or 2! was {}", size),
                ));
            } else {
                let mut res = vec![0; size * 16_384];
                file.read_exact(res.as_mut_slice())?;
                res
            }
        };

        let chrom = {
            let msb = (header[9] & 0xf0) as u16;
            let lsb = header[5] as u16;

            let size = Self::parse_size(lsb | msb);
            if size > 0 {
                let mut res = vec![0; size * 8192];
                file.read_exact(res.as_mut_slice())?;
                Some(res)
            } else {
                None
            }
        };

        let misc = {
            let mut res = Vec::new();
            file.read_to_end(&mut res)?;
            if res.len() > 0 {
                Some(res)
            } else {
                None
            }
        };

        Ok(ROM {
            header,
            prg_rom,
            chrom,
            trainer,
            misc,
            bus,
        })
    }

    pub fn load_prg(&mut self) {
        match self.prg_size() {
            2 => self
                .bus
                .borrow_mut()
                .write_range(0x8000, self.prg_rom.as_slice()),
            1 => {
                let mut bus = self.bus.borrow_mut();
                bus.write_range(0x8000, self.prg_rom.as_slice());
                bus.write_range(0xc000, self.prg_rom.as_slice());
            }
            _ => (),
        };
    }

    pub fn has_trainer(&self) -> bool {
        self.trainer.is_some()
    }

    pub fn prg_rom(&self) -> &[u8] {
        self.prg_rom.as_slice()
    }

    pub fn prg_size(&self) -> usize {
        // 16Kb chunks
        self.prg_rom.len() / 16_384
    }

    pub fn chrom_size(&self) -> usize {
        if let Some(ref chrom) = self.chrom {
            chrom.len() / 8192
        } else {
            0
        }
    }

    pub fn misc_size(&self) -> usize {
        if let Some(ref misc) = self.misc {
            misc.len()
        } else {
            0
        }
    }

    fn parse_size(s: u16) -> usize {
        if s >> 8 == 0xf {
            // exponent version
            let e = s & 0x3;
            let mm = (s ^ 0xf00) & !0x3;
            (2u16.pow(e as u32) * (mm * 2 + 1)) as usize
        } else {
            s as usize
        }
    }
}

struct NES {
    pub cpu: CPU,
    pub rom: ROM,
}

impl NES {
    fn run(&mut self) {
        loop {
            self.cpu.clock();
        }
    }

    fn new<P: AsRef<Path>>(rom_path: P) -> std::io::Result<Self> {
        let bus = Rc::new(RefCell::new(Bus {
            memory: [0; 64 * 1024],
        }));

        let mut rom = ROM::from_file(bus.clone(), File::open(rom_path)?)?;
        rom.load_prg();

        let cpu = CPU::new(bus.clone(), bus.borrow().read_u16(0xfffc));

        Ok(NES { cpu, rom })
    }
}

fn main() -> std::io::Result<()> {
    let mut n = NES::new("/home/james/Downloads/tutor.nes")?;
    n.run();

    Ok(())
}
