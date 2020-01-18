use super::*;

use std::fs::File;
use std::path::Path;

use ggez::event::{EventHandler, KeyCode};
use ggez::graphics::{self, DrawParam, Drawable, MeshBuilder, Text, TextFragment};
use ggez::input::keyboard::KeyMods;
use ggez::GameResult;

#[derive(Eq, PartialEq)]
enum RunState {
    Break,
    Step,
    Run,
}

pub struct NES {
    bus: Bus,
    cpu: CPU,
    ppu: PPU,

    run_state: RunState,
    disassembly: Vec<(u16, String)>,

    debug_ui: DebugUI,
}

impl NES {
    pub fn new<P: AsRef<Path>>(rom_path: P, ctx: &mut Context) -> std::io::Result<Self> {
        let cartridge = Cartridge::from_nes(File::open(rom_path)?)?;
        let bus = Bus::from_cartridge(cartridge);
        let cpu = CPU::new(bus.cpu_read::<u16>(0xfffc));

        Ok(NES {
            disassembly: cpu.disassemble(0x0000, 0xffff, &bus),
            ppu: PPU::new(),
            cpu: cpu,
            bus: bus,
            run_state: RunState::Break,
            debug_ui: DebugUI::new(ctx),
        })
    }

    fn _draw_bus(
        &self,
        ctx: &mut Context,
        mut addr: u16,
        rows: usize,
        cols: usize,
        pos: [f32; 2],
    ) -> GameResult<()> {
        let mut s = String::with_capacity(rows * (8 + (cols * 3)));

        for _ in 1..rows + 1 {
            s += &format!("${:04X}:  ", addr);

            for _ in 0..cols {
                s += &format!("{:02X} ", self.bus.cpu_read::<u8>(addr));
                addr += 1;
            }
            s += "\n";
        }

        Text::new(s).draw(ctx, DrawParam::new().dest([pos[0], pos[1]]))
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

        let mut txt = Text::default();
        for entry in &self.disassembly[idx.saturating_sub(10)..idx.saturating_add(10)] {
            let mut t = TextFragment::new(entry.1.clone());
            if entry.0 == self.cpu.pc {
                t = t.color([0.0, 1.0, 0.0, 1.0].into());
            }
            txt.add(t);
            txt.add("\n");
        }
        txt.draw(ctx, DrawParam::new().dest([pos[0], pos[1]]))?;

        Ok(())
    }

    fn draw_cpu(&self, ctx: &mut Context, pos: [f32; 2]) -> GameResult<()> {
        const LINE_HEIGHT: f32 = 20.0;
        let col1 = pos[0];
        let col2 = pos[0] + 80.0;

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

        let flags: Text = self.cpu.st.into();
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
                for _ in 0..CPU_HZ / 60 {
                    self.cpu.clock(&mut self.bus);
                    for _ in 0..3 {
                        self.ppu.clock(&mut self.bus)
                    }
                }
            }
            RunState::Break => (),
            RunState::Step => {
                self.cpu.clock(&mut self.bus);
                while self.cpu.cycles > 0 {
                    self.cpu.clock(&mut self.bus);
                    for _ in 0..3 {
                        self.ppu.clock(&mut self.bus)
                    }
                }

                self.run_state = RunState::Break;
            }
        };
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, graphics::BLACK);

        MeshBuilder::new()
            .rectangle(
                graphics::DrawMode::fill(),
                [SCREEN_W - MARGIN, 0.0, MARGIN, SCREEN_H].into(),
                [0.1, 0.1, 0.7, 1.0].into(),
            )
            .build(ctx)?
            .draw(ctx, DrawParam::new())?;

        self.ppu
            .draw(ctx, [0.0, 0.0], (SCREEN_W - MARGIN, SCREEN_H))?;
        /*
            self.ppu
            .draw_pattern_table(ctx, &self.bus, [SCREEN_W - MARGIN + 5.0, 500.0], 0, 1)?;
        self.ppu
            .draw_pattern_table(ctx, &self.bus, [SCREEN_W - MARGIN + 150.0, 500.0], 1, 1)?;
        self.draw_cpu(ctx, [SCREEN_W - MARGIN + 5.0, 0.0])?;
        self.draw_code(ctx, [SCREEN_W - MARGIN + 5.0, 150.0])?;
        */

        self.debug_ui.render(ctx);
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
    fn mouse_button_down_event(
        &mut self,
        _ctx: &mut Context,
        button: event::MouseButton,
        _x: f32,
        _y: f32,
    ) {
        self.debug_ui.update_mouse_down((
            button == event::MouseButton::Left,
            button == event::MouseButton::Right,
            button == event::MouseButton::Middle,
        ));
    }
    fn mouse_button_up_event(
        &mut self,
        _ctx: &mut Context,
        _button: event::MouseButton,
        _x: f32,
        _y: f32,
    ) {
        self.debug_ui.update_mouse_down((false, false, false))
    }
    fn mouse_motion_event(&mut self, _ctx: &mut Context, x: f32, y: f32, _dx: f32, _dy: f32) {
        self.debug_ui.update_mouse_pos(x, y)
    }
}
