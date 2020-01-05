mod registers;
pub use registers::*;

use std::cell::RefCell;
use std::rc::Rc;

use image::RgbaImage;

use ggez::graphics::{Color, DrawParam, Drawable, FilterMode, Image};
use ggez::{Context, GameResult};

use image::Rgba;

use super::bus::Bus;

const SCREEN_WIDTH: u32 = 256;
const SCREEN_HEIGHT: u32 = 240;

pub struct PPU {
    screen: RgbaImage,

    bus: Rc<RefCell<Bus>>,

    scanline: u32,
    cycle: u32,
}

impl PPU {
    const COLORS: [Rgba<u8>; 0x1f] = {
        let mut res = [Rgba([0, 0, 0, 0]); 0x1f];

        macro_rules! add_col {
            ($idx: literal, $r: literal, $g: literal, $b: literal) => {
                res[$idx] = Rgba([$r, $g, $b, 0xff]);
            };
        }

        add_col!(0x00, 0x75, 0x75, 0x75);
        add_col!(0x20, 0xFF, 0xFF, 0xFF);
        add_col!(0x01, 0x27, 0x1B, 0x8F);
        add_col!(0x21, 0x3F, 0xBF, 0xFF);
        add_col!(0x02, 0x00, 0x00, 0xAB);
        add_col!(0x22, 0x5F, 0x97, 0xFF);
        add_col!(0x03, 0x47, 0x00, 0x9F);
        add_col!(0x23, 0xA7, 0x8B, 0xFD);
        add_col!(0x04, 0x8F, 0x00, 0x77);
        add_col!(0x24, 0xF7, 0x7B, 0xFF);
        add_col!(0x05, 0xAB, 0x00, 0x13);
        add_col!(0x25, 0xFF, 0x77, 0xB7);
        add_col!(0x06, 0xA7, 0x00, 0x00);
        add_col!(0x26, 0xFF, 0x77, 0x63);
        add_col!(0x07, 0x7F, 0x0B, 0x00);
        add_col!(0x27, 0xFF, 0x9B, 0x3B);
        add_col!(0x08, 0x43, 0x2F, 0x00);
        add_col!(0x28, 0xF3, 0xBF, 0x3F);
        add_col!(0x09, 0x00, 0x47, 0x00);
        add_col!(0x29, 0x83, 0xD3, 0x13);
        add_col!(0x0A, 0x00, 0x51, 0x00);
        add_col!(0x2A, 0x4F, 0xDF, 0x4B);
        add_col!(0x0B, 0x00, 0x3F, 0x17);
        add_col!(0x2B, 0x58, 0xF8, 0x98);
        add_col!(0x0C, 0x1B, 0x3F, 0x5F);
        add_col!(0x2C, 0x00, 0xEB, 0xDB);
        add_col!(0x10, 0xBC, 0xBC, 0xBC);
        add_col!(0x30, 0xFF, 0xFF, 0xFF);
        add_col!(0x11, 0x00, 0x73, 0xEF);
        add_col!(0x31, 0xAB, 0xE7, 0xFF);
        add_col!(0x12, 0x23, 0x3B, 0xEF);
        add_col!(0x32, 0xC7, 0xD7, 0xFF);
        add_col!(0x13, 0x83, 0x00, 0xF3);
        add_col!(0x33, 0xD7, 0xCB, 0xFF);
        add_col!(0x14, 0xBF, 0x00, 0xBF);
        add_col!(0x34, 0xFF, 0xC7, 0xFF);
        add_col!(0x15, 0xE7, 0x00, 0x5B);
        add_col!(0x35, 0xFF, 0xC7, 0xDB);
        add_col!(0x16, 0xDB, 0x2B, 0x00);
        add_col!(0x36, 0xFF, 0xBF, 0xB3);
        add_col!(0x17, 0xCB, 0x4F, 0x0F);
        add_col!(0x37, 0xFF, 0xDB, 0xAB);
        add_col!(0x18, 0x8B, 0x73, 0x00);
        add_col!(0x38, 0xFF, 0xE7, 0xA3);
        add_col!(0x19, 0x00, 0x97, 0x00);
        add_col!(0x39, 0xE3, 0xFF, 0xA3);
        add_col!(0x1A, 0x00, 0xAB, 0x00);
        add_col!(0x3A, 0xAB, 0xF3, 0xBF);
        add_col!(0x1B, 0x00, 0x93, 0x3B);
        add_col!(0x3B, 0xB3, 0xFF, 0xCF);
        add_col!(0x1C, 0x00, 0x83, 0x8B);
        add_col!(0x3C, 0x9F, 0xFF, 0xF3);

        res
    };

    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        PPU {
            screen: RgbaImage::new(SCREEN_WIDTH, SCREEN_HEIGHT),
            bus: bus,

            scanline: 0,
            cycle: 0,
        }
    }

    pub fn get_color(&self, palette: u16, pixel: u16) -> Rgba<u8> {
        // Retrieving a color requires the palette id, and a pixel number
        let addr = 0x3f00 + (palette << 2) + pixel;
        Self::COLORS[self.bus.borrow().ppu_read::<u8>(addr) as usize]
    }

    pub fn clock(&mut self) {
        *self.screen.get_pixel_mut(self.cycle, self.scanline) = if rand::random() {
            image::Rgba([255, 255, 255, 255])
        } else {
            image::Rgba([0, 0, 0, 0])
        };

        self.cycle += 1;
        if self.cycle >= 256 {
            self.cycle = 0;
            self.scanline += 1;
            if self.scanline >= 240 {
                self.scanline = 0;
            }
        }
    }

    pub fn draw(&mut self, ctx: &mut Context, pos: [f32; 2], (w, h): (f32, f32)) -> GameResult<()> {
        let mut i = Image::from_rgba8(
            ctx,
            SCREEN_WIDTH as u16,
            SCREEN_HEIGHT as u16,
            &*self.screen,
        )?;
        i.set_filter(FilterMode::Nearest);

        i.draw(
            ctx,
            DrawParam::new().dest(pos).scale([w / 256.0, h / 240.0]),
        )
    }

    pub fn draw_pattern_table(
        &mut self,
        ctx: &mut Context,
        pos: [f32; 2],
        table: u8,
        palette: u8,
    ) -> GameResult<()> {
        Ok(())
    }
}
