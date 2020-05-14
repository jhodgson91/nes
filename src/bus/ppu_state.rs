use bitfield::*;

bitfield! {
    #[derive(Copy, Clone, Default)]
    pub struct LoopyRegister(u16);
    u8;
    pub coarse_x, set_coarse_x: 4,0;
    pub coarse_y, set_coarse_y: 9,5;
    pub nametable_x, set_nametable_x: 10;
    pub nametable_y, set_nametable_y: 11;
    pub fine_y, set_fine_y: 14,12;
    pub unused, _: 15;

    pub val, set_val: 15, 0;
}

bitfield! {
    #[derive(Copy, Clone, Default)]
    pub struct PPUCTRL(u8);
    pub u8, nametable_x, _: 0;
    pub u8, nametable_y, _: 1;
    pub increment_mode, _: 2;
    pub pattern_sprite, _: 3;
    pub pattern_background, _: 4;
    pub sprite_size, _: 5;
    pub slave_mode, _: 6;
    pub generate_nmi, _: 7;
}

bitfield! {
    #[derive(Copy, Clone, Default)]
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
    #[derive(Copy, Clone, Default)]
    pub struct PPUSTATUS(u8);
    pub unused, set_unused: 4, 0;
    pub sprite_overflow, _: 5;
    pub sprite_hit, _: 6;
    pub vblank, set_vblank: 7;
}

#[derive(Copy, Clone, Default)]
pub struct PPUState {
    pub ctrl: PPUCTRL,
    pub mask: PPUMASK,
    pub status: PPUSTATUS,

    pub vram_addr: LoopyRegister,
    pub tram_addr: LoopyRegister,

    pub fine_x: u8,

    pub data_buffer: u8,
    pub addr_latch: bool,
}
