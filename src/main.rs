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

use ggez::event::{self, EventHandler, KeyCode};
use ggez::graphics::{self, DrawParam, Drawable, Text, TextFragment};
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
    pub bus: Rc<RefCell<Bus>>,

    disassembly: Vec<(u16, String)>,
}

impl NES {
    fn draw_bus(
        &self,
        ctx: &mut Context,
        mut addr: u16,
        rows: u8,
        cols: u8,
        pos: [f32; 2],
    ) -> GameResult<()> {
        const LINE: f32 = 20.0;
        const SPACE: f32 = 25.0;

        let bus = self.bus.borrow();

        for i in 0..cols {
            Text::new(format!("{:02X}", i)).draw(
                ctx,
                DrawParam::new().dest([pos[0] + 60.0 + (SPACE * i as f32), pos[1]]),
            )?;
        }

        for r in 1..rows + 1 {
            Text::new(format!("${:04X}:", addr)).draw(
                ctx,
                DrawParam::default().dest([pos[0], pos[1] + (LINE * r as f32)]),
            )?;

            for c in 0..cols {
                Text::new(format!("{:02X} ", bus.cpu_read::<u8>(addr))).draw(
                    ctx,
                    DrawParam::new().dest([
                        pos[0] + 60.0 + (SPACE * c as f32),
                        pos[1] + (LINE * r as f32),
                    ]),
                )?;
                addr += 1;
            }
        }

        Ok(())
    }

    fn draw_code(&self, ctx: &mut Context, pos: [f32; 2]) -> GameResult<()> {
        let idx = self
            .disassembly
            .iter()
            .position(|(pc, _)| *pc == self.cpu.pc)
            .ok_or(ggez::GameError::RenderError(format!(
                "Disassembly does not contain pc {:04X}",
                self.cpu.pc
            )))?;

        for (i, entry) in self.disassembly[idx.saturating_sub(10)..idx.saturating_add(10)]
            .iter()
            .enumerate()
        {
            let mut t = TextFragment::new(entry.1.clone());
            if entry.0 == self.cpu.pc {
                t = t.color([0.0, 1.0, 0.0, 1.0].into());
            }

            Text::new(t).draw(
                ctx,
                DrawParam::new().dest([pos[0], pos[1] + i as f32 * 20.0]),
            )?;
        }

        Ok(())
    }

    fn draw_cpu(&self, ctx: &mut Context, pos: [f32; 2]) -> GameResult<()> {
        const LINE_HEIGHT: f32 = 20.0;
        let col1 = pos[0];
        let col2 = pos[0] + 80.0;

        macro_rules! flag_text {
            ($f: ident) => {
                TextFragment::new(stringify!($f)).color(if self.cpu.get_flag(CPU::$f) {
                    [0.0, 1.0, 0.0, 1.0].into()
                } else {
                    [1.0, 0.0, 0.0, 1.0].into()
                });
            };
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

        let flags = {
            let mut res = Text::default();
            res.add(flag_text!(N));
            res.add(TextFragment::new("-"));
            res.add(flag_text!(V));
            res.add(flag_text!(B));
            res.add(flag_text!(D));
            res.add(flag_text!(I));
            res.add(flag_text!(Z));
            res.add(flag_text!(C));
            res
        };

        draw_two_col!("Status:", flags, 0);
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
        draw_two_col!(
            "SP:",
            Text::new(format!(
                "${:02X} [${:04X}]",
                self.cpu.sp,
                self.cpu.stack_addr()
            )),
            5
        );

        Ok(())
    }
}

impl EventHandler for NES {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        match self.run_state {
            RunState::Run => {
                for _i in 0..CPU_HZ / 60 {
                    self.cpu.clock();
                }
            }
            RunState::Break => (),
            RunState::Step => {
                while self.cpu.cycles > 0 {
                    self.cpu.clock()
                }
                self.cpu.clock();

                self.run_state = RunState::Break;
            }
        };
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, graphics::Color::new(0.1, 0.1, 0.7, 1.0));
        self.draw_bus(ctx, 0x1f0, 1, 16, [0.0, 0.0])?;
        self.draw_bus(ctx, 0x0, 2, 16, [0.0, 100.0])?;
        self.draw_cpu(ctx, [550.0, 0.0])?;
        self.draw_code(ctx, [550.0, 150.0])?;
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
            KeyCode::Space => {
                if self.run_state == RunState::Break {
                    self.run_state = RunState::Step;
                }
            }
            KeyCode::I => {
                self.cpu.irq();
                if self.run_state == RunState::Break {
                    self.run_state = RunState::Step;
                }
            }
            KeyCode::N => {
                self.cpu.nmi();
                if self.run_state == RunState::Break {
                    self.run_state = RunState::Step;
                }
            }
            KeyCode::R => {
                self.cpu.reset();
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
        let bus = Rc::new(RefCell::new(Bus::new(cartridge)));
        let cpu = CPU::new(bus.clone());

        Ok(NES {
            disassembly: cpu.disassemble(0x0000, 0xffff),
            run_state: RunState::Break,
            cpu,
            bus,
        })
    }
}

fn main() -> std::io::Result<()> {
    let mut nes = NES::new("/home/james/Projects/rust/nes/roms/test.nes")?;
    let (mut ctx, mut event_loop) = ContextBuilder::new("NES", "jhodgson").build().unwrap();

    match event::run(&mut ctx, &mut event_loop, &mut nes) {
        Ok(_) => (),
        Err(e) => println!("Error occured: {}", e),
    };

    Ok(())
}
