use std::iter::zip;

use chip8_core::{FRAME_HEIGHT, FRAME_WIDTH};
use pixels::Pixels;
use self_cell::self_cell;
use winit::window::Window;

use crate::colour::{Colour, ColourPalette};

// Pixels<'window> references Window.
self_cell!(
    pub struct Display {
        owner: Window,
        #[covariant]
        dependent: Pixels,
    }
);

impl Display {
    pub fn create(
        event_loop: &winit::event_loop::ActiveEventLoop,
        colour_palette: &ColourPalette,
    ) -> Self {
        let window = event_loop
            .create_window(winit::window::Window::default_attributes())
            .unwrap();

        Self::new(window, |window| {
            let size = window.inner_size();
            let surface = pixels::SurfaceTexture::new(size.width, size.height, window);
            let mut pixels =
                Pixels::new(u32::from(FRAME_WIDTH), u32::from(FRAME_HEIGHT), surface).unwrap();
            pixels.clear_color(pixels::wgpu::Color::from(colour_palette.letterbox_colour));

            pixels
        })
    }

    fn window(&self) -> &Window {
        self.borrow_owner()
    }

    pub fn request_redraw(&self) {
        self.window().request_redraw();
    }

    pub fn window_id(&self) -> winit::window::WindowId {
        self.window().id()
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        self.with_dependent_mut(|_window, pixels| {
            pixels.resize_surface(width, height).unwrap();
        });
    }

    pub fn render(&mut self, rendered_pixel_colours: &[Colour]) {
        self.with_dependent_mut(|_window, pixels| {
            let frame = pixels.frame_mut();
            for (dst, pixel_colour) in zip(frame.chunks_exact_mut(4), rendered_pixel_colours) {
                dst.copy_from_slice(&pixel_colour.as_rgba());
            }
            pixels.render().unwrap();
        });
    }
}
