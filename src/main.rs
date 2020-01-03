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

use ggez::conf::WindowSetup;
use ggez::event::{self, EventHandler, KeyCode};
use ggez::graphics::{self, Color, DrawParam, Drawable, Text, TextFragment};
use ggez::input::keyboard::KeyMods;
use ggez::{Context, ContextBuilder, GameResult};

const CPU_HZ: u32 = 1789773; // cycles per second

#[derive(Eq, PartialEq)]
enum RunState {
    Break,
    Step,
    Run,
}

struct NES {
    run_state: RunState,

    pub cpu: CPU,
}

pub const RED: Color = Color {
    r: 1.0,
    g: 0.0,
    b: 0.0,
    a: 1.0,
};

pub const GREEN: Color = Color {
    r: 0.0,
    g: 1.0,
    b: 0.0,
    a: 1.0,
};

impl NES {
    fn draw_cpu(&self, ctx: &mut Context, pos: [f32; 2]) -> GameResult<()> {
        const LINE_HEIGHT: f32 = 20.0;
        let col1 = pos[0];
        let col2 = pos[0] + 100.0;

        macro_rules! flags_text {
            ($($f: ident),*) => {{
                let mut t = Text::default();
                $(
                    let frag = TextFragment::new(stringify!($f)).color(if self.cpu.get_flag(CPU::$f) {GREEN} else {RED});
                    t.add(frag);
                )*
                t
            }};
        }

        macro_rules! draw_two_col {
            ($first: expr, $second: expr, $line: literal) => {
                Text::new($first).draw(
                    ctx,
                    DrawParam::default().dest([col1, $line as f32 * LINE_HEIGHT]),
                )?;
                $second.draw(
                    ctx,
                    DrawParam::default().dest([col2, $line as f32 * LINE_HEIGHT]),
                )?;
            };
        }

        draw_two_col!("Status:", flags_text!(N, V, B, D, I, Z, C), 0);
        draw_two_col!("PC:", Text::new(format!("${:04X}", self.cpu.pc)), 1);
        draw_two_col!(
            "A:",
            Text::new(format!("${:02X}  [{:03}]", self.cpu.a, self.cpu.a)),
            2
        );
        draw_two_col!(
            "X:",
            Text::new(format!("${:02X}  [{:03}]", self.cpu.x, self.cpu.x)),
            3
        );
        draw_two_col!(
            "Y:",
            Text::new(format!("${:02X}  [{:03}]", self.cpu.y, self.cpu.y)),
            4
        );

        Ok(())
    }
}

impl EventHandler for NES {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        match self.run_state {
            RunState::Run => {
                let mut cycles = 0;

                // TODO - Incorrect, need to run as many cycles as fit into the frame!
                // Does for now. We start in the Break state so this doesn't run immediately!
                while cycles < CPU_HZ {
                    self.cpu.clock();
                    cycles += 1;
                }
            }
            RunState::Break => (),
            RunState::Step => {
                self.cpu.clock();
                while self.cpu.instruction.cycles > 0 {
                    self.cpu.clock()
                }

                self.run_state = RunState::Break;
            }
        };
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, graphics::Color::new(0.1, 0.1, 0.7, 1.0));
        self.draw_cpu(ctx, [600.0, 0.0])?;
        graphics::present(ctx)?;
        Ok(())
    }

    fn key_down_event(
        &mut self,
        _ctx: &mut Context,
        keycode: KeyCode,
        _keymods: KeyMods,
        _repeat: bool,
    ) {
        match keycode {
            KeyCode::B => self.run_state = RunState::Break,
            KeyCode::N => {
                if self.run_state == RunState::Break {
                    self.run_state = RunState::Step;
                }
            }
            KeyCode::C => self.run_state = RunState::Run,
            _ => (),
        };
    }
}

impl NES {
    fn new<P: AsRef<Path>>(rom_path: P) -> std::io::Result<Self> {
        let cartridge = Cartridge::from_nes(File::open(rom_path)?)?;
        let cpu = CPU::new(Rc::new(RefCell::new(Bus::new(cartridge))));
        Ok(NES {
            run_state: RunState::Break,
            cpu,
        })
    }
}

fn main() -> std::io::Result<()> {
    let mut nes = NES::new("/home/james/Projects/rust/nes/roms/implied.nes")?;
    let (mut ctx, mut event_loop) = ContextBuilder::new("NES", "jhodgson")
        .window_setup(WindowSetup {
            vsync: false,
            ..Default::default()
        })
        .build()
        .unwrap();

    match event::run(&mut ctx, &mut event_loop, &mut nes) {
        Ok(_) => (),
        Err(e) => println!("Error occured: {}", e),
    };

    Ok(())
}
