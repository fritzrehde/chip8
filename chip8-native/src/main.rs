mod utils;

use std::{
    iter::zip,
    num::NonZeroU32,
    path::PathBuf,
    time::{Duration, Instant},
};

use chip8_core::{Cpu, FRAME_HEIGHT, FRAME_WIDTH, Pixel, Rom};
use clap::Parser;
use nonzero_ext::nonzero;

#[derive(Debug, Parser)]
struct Cli {
    rom_file_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let rom = Rom::read_from_file(cli.rom_file_path)?;
    let cpu = Cpu::new();
    let mut app = App::new(cpu, &rom);

    let event_loop = winit::event_loop::EventLoop::new()?;
    event_loop.run_app(&mut app)?;

    Ok(())
}

// Fixed, required by CHIP-8.
const TICKS_PER_SEC: f64 = 60.0;
// TODO: make user-customizable
const INSTS_PER_SEC: f64 = 700.0;
const FRAMES_PER_SEC: f64 = 60.0;

struct App {
    cpu: Cpu,
    render: [u32; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
    /// Window to display frame; only exists once "resumed" by OS, which only happens
    /// at startup for desktop apps (mobile apps can be suspended and resumed arbitrarily,
    /// but we only support desktop usage).
    graphics: Option<Graphics>,
    /// Deadline for the next tick.
    next_tick: Instant,
    /// Deadline for the next instruction execution.
    next_inst: Instant,
    /// Deadline for requesting the next frame to be drawn.
    next_frame: Instant,
    time_between_ticks: Duration,
    time_between_insts: Duration,
    time_between_frames: Duration,
    colour_palette: ColourPalette,
}

impl App {
    fn new(mut cpu: Cpu, rom: &Rom) -> Self {
        cpu.load_rom(rom);

        let time_between_ticks = Duration::from_secs_f64(1.0 / TICKS_PER_SEC);
        let time_between_insts = Duration::from_secs_f64(1.0 / INSTS_PER_SEC);
        let time_between_frames = Duration::from_secs_f64(1.0 / FRAMES_PER_SEC);

        let now = Instant::now();

        Self {
            cpu,
            render: [0x0; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
            graphics: None,
            next_tick: now + time_between_ticks,
            next_inst: now + time_between_insts,
            next_frame: now + time_between_frames,
            time_between_ticks,
            time_between_insts,
            time_between_frames,
            colour_palette: Default::default(),
        }
    }

    fn tick_or_step_or_frame(&mut self) {
        let now = Instant::now();

        while now >= self.next_tick {
            self.cpu.tick();
            self.next_tick += self.time_between_ticks;
        }

        while now >= self.next_inst {
            self.cpu.step();
            self.next_inst += self.time_between_insts;
        }

        let mut request_redraw = false;
        while now >= self.next_frame {
            request_redraw = true;
            self.next_frame += self.time_between_frames;
        }
        if request_redraw {
            self.request_window_redraw();
        }
    }

    fn request_window_redraw(&self) {
        let Some(graphics) = &self.graphics else {
            return;
        };
        graphics.window().request_redraw();
    }

    fn next_deadline(&self) -> Instant {
        min!(self.next_tick, self.next_inst, self.next_frame)
    }

    // TODO: should only be called if sth in frame has changed since last draw, for improved perf
    fn draw(&mut self) {
        let Some(graphics) = &mut self.graphics else {
            return;
        };
        // TODO: converting Pixels to u32 hex codes on every render is inefficient, think about maintaining duplicate u32 hex state alongside Pixel array
        for (rendered_pixel, pixel) in zip(self.render.iter_mut(), self.cpu.framebuffer().iter()) {
            *rendered_pixel = match pixel {
                Pixel::Filled => self.colour_palette.foreground_colour,
                Pixel::Empty => self.colour_palette.background_colour,
            };
        }
        graphics.render(&self.render);
    }
}

impl winit::application::ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.graphics = Some(Graphics::new(event_loop));
    }

    fn new_events(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        cause: winit::event::StartCause,
    ) {
        match cause {
            winit::event::StartCause::ResumeTimeReached {
                start: _,
                requested_resume: _,
            } => {
                self.tick_or_step_or_frame();
                event_loop.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(
                    self.next_deadline(),
                ));
            }
            winit::event::StartCause::WaitCancelled {
                start: _,
                requested_resume: _,
            } => {
                // Re-request the cancelled wait request.
                event_loop.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(
                    self.next_deadline(),
                ));
            }
            _ => {}
        }
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        let Some(graphics) = &mut self.graphics else {
            return;
        };
        if graphics.window().id() != window_id {
            return;
        }
        match event {
            winit::event::WindowEvent::RedrawRequested => self.draw(),
            // winit::event::WindowEvent::KeyboardInput {
            //     device_id,
            //     event,
            //     is_synthetic,
            // } => todo!(),
            winit::event::WindowEvent::CloseRequested => event_loop.exit(),
            _ => {}
        };
    }
}

// TODO: choose better name
struct Graphics {
    surface: softbuffer::Surface<winit::event_loop::OwnedDisplayHandle, winit::window::Window>,
}

impl Graphics {
    fn new(event_loop: &winit::event_loop::ActiveEventLoop) -> Self {
        let window = event_loop
            .create_window(winit::window::Window::default_attributes())
            .unwrap();
        let context = softbuffer::Context::new(event_loop.owned_display_handle()).unwrap();
        let mut surface = softbuffer::Surface::new(&context, window).unwrap();
        surface
            .resize(
                NonZeroU32::from(nonzero!(FRAME_WIDTH)),
                NonZeroU32::from(nonzero!(FRAME_HEIGHT)),
            )
            .unwrap();
        Self { surface }
    }

    fn render(&mut self, render: &[u32]) {
        let mut buf = self.surface.buffer_mut().unwrap();
        buf.copy_from_slice(render);
        buf.present().unwrap();
    }

    fn window(&self) -> &winit::window::Window {
        self.surface.window()
    }
}

struct ColourPalette {
    foreground_colour: u32,
    background_colour: u32,
}

const BLACK_RGB: u32 = 0x00_00_00;
const WHITE_RGB: u32 = 0xFF_FF_FF;

impl Default for ColourPalette {
    fn default() -> Self {
        Self {
            background_colour: BLACK_RGB,
            foreground_colour: WHITE_RGB,
        }
    }
}
