use std::fs::File;
use std::io::Read;
use std::io::{Seek, SeekFrom};

pub struct Cartridge {
    pub sram: [u8; 8 * 1024],

    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
}

impl Cartridge {
    pub fn from_nes(mut file: File) -> std::io::Result<Self> {
        let mut header = [0u8; 16];
        file.read_exact(&mut header).unwrap();

        // Skip trainer if any exists
        if header[6] & 0x40 != 0 {
            file.seek(SeekFrom::Current(512))?;
        }

        // We're assuming NES 2.0 format here...
        let prg_rom = {
            // first 4 bits of byte 9 == msb
            let msb = (header[9] & 0xf) as u16;
            // byte 4 == lsb
            let lsb = header[4] as u16;

            let size = Self::parse_size(lsb | msb);
            let mut res = vec![0; size * 16_384];
            file.read_exact(res.as_mut_slice())?;
            res
        };

        let chr_rom = {
            let msb = (header[9] & 0xf0) as u16;
            let lsb = header[5] as u16;

            let size = Self::parse_size(lsb | msb);
            let mut res = vec![0; size * 8192];
            file.read_exact(res.as_mut_slice())?;
            res
        };

        Ok(Cartridge {
            sram: [0; 8 * 1024],
            prg_rom,
            chr_rom,
        })
    }

    pub fn map(&self, addr: u16) -> u16 {
        match self.prg_size() {
            2 => addr & 0x7fff,
            1 => addr & 0x3fff,
            _ => panic!("Unsupported number of prg banks!"),
        }
    }

    pub fn prg_size(&self) -> usize {
        // 16Kb chunks
        self.prg_rom.len() / 16_384
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
