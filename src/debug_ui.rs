use ggez::graphics;
use ggez::Context;

use gfx_core::{handle::RenderTargetView, memory::Typed};
use gfx_device_gl;

use imgui::*;
use imgui_gfx_renderer::*;

#[derive(Copy, Clone, PartialEq, Debug, Default)]
struct MouseState {
    pos: (i32, i32),
    pressed: (bool, bool, bool),
    wheel: f32,
}

pub struct DebugUI {
    pub imgui: imgui::Context,
    pub renderer: Renderer<gfx_core::format::Rgba8, gfx_device_gl::Resources>,

    mouse_state: MouseState,
}

impl DebugUI {
    pub fn new(ctx: &mut Context) -> Self {
        let mut imgui = imgui::Context::create();
        let (factory, device, _, _, _) = graphics::gfx_objects(ctx);

        let shaders = {
            let version = device.get_info().shading_language;
            if version.is_embedded {
                if version.major >= 3 {
                    Shaders::GlSlEs300
                } else {
                    Shaders::GlSlEs100
                }
            } else if version.major >= 4 {
                Shaders::GlSl400
            } else if version.major >= 3 {
                Shaders::GlSl130
            } else {
                Shaders::GlSl110
            }
        };

        // Renderer
        let renderer = Renderer::init(&mut imgui, &mut *factory, shaders).unwrap();

        Self {
            imgui,
            renderer,
            mouse_state: MouseState::default(),
        }
    }

    pub fn render(&mut self, ctx: &mut Context) {
        self.update_mouse();

        let (w, h) = graphics::drawable_size(ctx);
        let delta = ggez::timer::delta(ctx);
        let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;

        self.imgui.io_mut().display_size = [w, h];
        self.imgui.io_mut().delta_time = delta_s;

        let ui = self.imgui.frame();

        imgui::Window::new(im_str!("Debugger"))
            .save_settings(false)
            .position([w - 400.0, 0.0], imgui::Condition::FirstUseEver)
            .size([400.0, h], Condition::FirstUseEver)
            .resizable(false)
            .movable(false)
            .build(&ui, || {
                if ui.collapsing_header(im_str!("CPU")).build() {
                    ui.text("Hiiii");
                }
            });

        // Render
        let (factory, _, encoder, _, render_target) = graphics::gfx_objects(ctx);
        let draw_data = ui.render();
        self.renderer
            .render(
                &mut *factory,
                encoder,
                &mut RenderTargetView::new(render_target.clone()),
                draw_data,
            )
            .unwrap();
    }

    fn update_mouse(&mut self) {
        self.imgui.io_mut().mouse_pos =
            [self.mouse_state.pos.0 as f32, self.mouse_state.pos.1 as f32];

        self.imgui.io_mut().mouse_down = [
            self.mouse_state.pressed.0,
            self.mouse_state.pressed.1,
            self.mouse_state.pressed.2,
            false,
            false,
        ];

        self.imgui.io_mut().mouse_wheel = self.mouse_state.wheel;
        self.mouse_state.wheel = 0.0;
    }

    pub fn update_mouse_pos(&mut self, x: f32, y: f32) {
        self.mouse_state.pos = (x as i32, y as i32);
    }

    pub fn update_mouse_down(&mut self, pressed: (bool, bool, bool)) {
        self.mouse_state.pressed = pressed;
    }
}
