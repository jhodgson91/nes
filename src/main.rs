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
use std::path::Path;

struct NES {
    pub cpu: CPU,
}

impl NES {
    fn run(&mut self) {
        loop {
            self.cpu.clock();
        }
    }

    fn new<P: AsRef<Path>>(rom_path: P) -> std::io::Result<Self> {
        let cartridge = Cartridge::from_file(File::open(rom_path)?)?;
        let cpu = CPU::new(Rc::new(RefCell::new(Bus::new(cartridge))));
        Ok(NES { cpu })
    }
}

fn main() -> std::io::Result<()> {
    let mut n = NES::new("/home/james/Projects/rust/nes/roms/implied.nes")?;
    n.run();

    Ok(())
}
