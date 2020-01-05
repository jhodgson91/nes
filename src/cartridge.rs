use std::fs::File;
use std::io::Read;
use std::io::{Seek, SeekFrom};

use bitfield::*;

bitfield! {
    // http://wiki.nesdev.com/w/index.php/NES_2.0
    pub struct Header([u8]);
    u8;

    pub u32, nes_id, _: 31, 0;

    u16, prg_size_lsb, _: 39, 32;
    u16, chr_size_lsb, _: 47, 40;

    pub mirroring, _: 48;
    pub has_battery_mem, _: 89;
    pub has_trainer, _: 50;
    pub four_screen_mode, _: 51;
    pub mapper1, _: 55 ,52;

    pub console_type, _: 57, 56;
    pub id_num, _: 59, 58;
    pub mapper2, _: 63, 60;

    pub mapper3, _: 68, 65;

    pub sub_mapper, _: 72,69;

    u16, prg_size_msb, _: 76, 73;
    u16, chr_size_msb, _: 80, 77;
}

impl<T: AsMut<[u8]> + AsRef<[u8]>> Header<T> {
    pub fn prg_size(&self) -> usize {
        Self::parse_size(self.prg_size_msb() << 8 | self.prg_size_lsb())
    }
    pub fn chr_size(&self) -> usize {
        Self::parse_size(self.chr_size_msb() << 8 | self.chr_size_lsb())
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

pub struct Cartridge {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,

    pub sram: [u8; 8 * 1024],
}

impl Cartridge {
    pub fn from_nes(mut file: File) -> std::io::Result<Self> {
        let mut header = Header([0u8; 16]);

        file.read_exact(&mut header.0).unwrap();

        // Skip trainer if any exists
        if header.has_trainer() {
            file.seek(SeekFrom::Current(512))?;
        }

        // We're assuming NES 2.0 format here...
        let prg_rom = {
            let size = header.prg_size();
            let mut res = vec![0; size * 16_384];
            file.read_exact(res.as_mut_slice())?;
            res
        };

        let chr_rom = {
            let size = header.chr_size();

            if size > 0 {
                let mut res = vec![0; size * 8192];
                file.read_exact(res.as_mut_slice())?;
                res
            } else {
                vec![0; 2 * 8192]
            }
        };

        Ok(Cartridge {
            sram: [0; 8 * 1024],
            prg_rom,
            chr_rom,
        })
    }

    pub fn cpu_map(&self, addr: u16) -> u16 {
        match addr {
            0x8000..=0xffff => {
                addr & match self.prg_size() {
                    2 => 0x7fff,
                    1 => 0x3fff,
                    _ => panic!("Unsupported no. of prg banks!"),
                }
            }
            _ => addr,
        }
    }

    pub fn prg_size(&self) -> usize {
        // 16Kb chunks
        self.prg_rom.len() / 16_384
    }
}
