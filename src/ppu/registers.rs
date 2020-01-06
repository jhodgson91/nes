use bitfield::*;

bitfield! {
    pub struct PPUCTRL(u8);
    u16, nametable_id, _: 1,0;
    pub increment_mode, _: 2;
    pattern_sprite, _: 3;
    pattern_background, _: 4;
    pub sprite_size, _: 5;
    pub slave_mode, _: 6;
    pub generate_nmi, _: 7;
}

impl PPUCTRL {
    fn _nametable_addr(&self) -> u16 {
        0x2000 + self.nametable_id() * 0x400
    }

    fn _sprite_pattern_addr(&self) -> u16 {
        self.pattern_sprite() as u16 * 0x1000
    }

    fn _background_pattern_addr(&self) -> u16 {
        self.pattern_background() as u16 * 0x1000
    }
}

bitfield! {
    pub struct PPUMASK(u8);
    pub greyscale, _: 0;
    pub render_background_left, _: 1;
    pub render_sprites_left, _: 2;
    pub show_background, _: 3;
    pub show_sprites, _: 4;
    pub emphasize_red, _: 5;
    pub emphasize_green, _: 6;
    pub emphasize_blue, _: 7;
}

bitfield! {
    pub struct PPUSTATUS(u8);
    pub sprite_overflow, _: 5;
    pub sprite_hit, _: 6;
    pub vblank, _: 7;
}
