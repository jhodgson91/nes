mod bus;
mod cartridge;
mod cpu;
mod nes;
mod ppu;

mod debug_ui;

pub use bus::Bus;
pub use cartridge::Cartridge;
pub use cpu::CPU;
pub use debug_ui::DebugUI;
pub use nes::NES;
pub use ppu::PPU;

use ggez::conf::WindowMode;
use ggez::event;
use ggez::{Context, ContextBuilder};

const CPU_HZ: u32 = 1789773; // cycles per second

const SCREEN_W: f32 = 1400.0;
const SCREEN_H: f32 = 1000.0;
const MARGIN: f32 = 400.0;

fn main() -> std::io::Result<()> {
    let (mut ctx, mut event_loop) = ContextBuilder::new("NES", "jhodgson")
        .window_mode(WindowMode {
            width: SCREEN_W,
            height: SCREEN_H,
            ..Default::default()
        })
        .build()
        .unwrap();

    let mut nes = NES::new("/home/james/Projects/rust/nes/roms/nestest.nes", &mut ctx)?;

    match event::run(&mut ctx, &mut event_loop, &mut nes) {
        Ok(_) => (),
        Err(e) => println!("Error occured: {}", e),
    };

    Ok(())
}
