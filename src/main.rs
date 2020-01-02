mod bus;
mod cartridge;
mod cpu;

use bus::Bus;
use cartridge::Cartridge;
use cpu::CPU;

use std::cell::RefCell;
use std::rc::Rc;

use std::convert::AsRef;
use std::fs::File;
use std::io::{Error, ErrorKind, Read};
use std::path::Path;

struct NES {
    pub cpu: CPU,
    pub rom: Cartridge,
}

impl NES {
    fn run(&mut self) {
        loop {
            self.cpu.clock();
        }
    }

    fn new<P: AsRef<Path>>(rom_path: P) -> std::io::Result<Self> {
        let bus: Rc<RefCell<Bus>> = Rc::default();

        let mut rom = Cartridge::from_file(bus.clone(), File::open(rom_path)?)?;
        rom.load_prg();

        let cpu = CPU::new(bus.clone());

        Ok(NES { cpu, rom })
    }
}

fn main() -> std::io::Result<()> {
    let mut n = NES::new("/home/james/Projects/rust/nes/roms/implied.nes")?;
    n.run();

    Ok(())
}
