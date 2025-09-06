mod utils;

use std::{
    iter::zip,
    num::NonZeroU32,
    path::PathBuf,
    time::{Duration, Instant},
};

use chip8_core::{Cpu, CpuState, FRAME_HEIGHT, FRAME_WIDTH, Pixel, Rom};
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
    /// Deadline for the next CPU tick.
    next_tick: Instant,
    /// Deadline for executing the next instruction.
    next_inst: Instant,
    /// Deadline for requesting the next frame to be drawn.
    next_frame: Instant,
    time_between_ticks: Duration,
    time_between_insts: Duration,
    time_between_frames: Duration,
    colour_palette: ColourPalette,
    beep_sound: BeepSound,
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
            beep_sound: BeepSound::new(),
        }
    }

    fn tick(&mut self) {
        let now = Instant::now();
        while now >= self.next_tick {
            match self.cpu.tick() {
                chip8_core::ActionOnTick::PlayBeepSound => self.beep_sound.on(),
                chip8_core::ActionOnTick::PauseBeepSound => self.beep_sound.off(),
            }
            self.next_tick += self.time_between_ticks;
        }
    }

    fn step(&mut self) {
        let now = Instant::now();
        while now >= self.next_inst {
            self.cpu.step();
            self.next_inst += self.time_between_insts;
        }
    }

    fn frame(&mut self) {
        let now = Instant::now();
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

// While the cpu is blocked waiting for input, no instructions
// need to be be executed, which means the frame will also not
// be updated. Cpu must continue to tick at all times, though.
impl App {
    fn tick_and_step_and_frame(&mut self) {
        match self.cpu.state() {
            CpuState::Executing => {
                self.tick();
                self.step();
                self.frame();
            }
            CpuState::WaitingForInput { .. } => {
                self.tick();
            }
        }
    }

    fn next_deadline(&self) -> Instant {
        match self.cpu.state() {
            CpuState::Executing => min!(self.next_tick, self.next_inst, self.next_frame),
            CpuState::WaitingForInput { .. } => {
                min!(self.next_tick)
            }
        }
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
                self.tick_and_step_and_frame();
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
            winit::event::WindowEvent::KeyboardInput {
                device_id: _,
                event,
                is_synthetic: _,
            } => {
                let winit::keyboard::PhysicalKey::Code(key_code) = event.physical_key else {
                    return;
                };
                let Some(key) = key_map(key_code) else {
                    return;
                };
                let key_state = key_state_map(event.state);
                match self.cpu.update_key_state(key, key_state) {
                    chip8_core::CpuStateChange::WaitingToExecuting => {
                        // Remove all instruction- and frame-"debt" collected
                        // during wait, to avoid catch-up.
                        let now = Instant::now();
                        self.next_inst = now + self.time_between_insts;
                        self.next_frame = now + self.time_between_frames;
                    }
                    chip8_core::CpuStateChange::NoChange => {}
                };
            }
            winit::event::WindowEvent::CloseRequested => event_loop.exit(),
            _ => {}
        };
    }
}

fn key_map(winit_keycode: winit::keyboard::KeyCode) -> Option<chip8_core::Key> {
    // PC:        CHIP-8:
    // 1 2 3 4    1 2 3 C
    // Q W E R -> 4 5 6 D
    // A S D F    7 8 9 E
    // Z X C V    A 0 B F

    match winit_keycode {
        winit::keyboard::KeyCode::Digit1 => Some(chip8_core::Key::new(0x1)),
        winit::keyboard::KeyCode::Digit2 => Some(chip8_core::Key::new(0x2)),
        winit::keyboard::KeyCode::Digit3 => Some(chip8_core::Key::new(0x3)),
        winit::keyboard::KeyCode::Digit4 => Some(chip8_core::Key::new(0xC)),
        winit::keyboard::KeyCode::KeyQ => Some(chip8_core::Key::new(0x4)),
        winit::keyboard::KeyCode::KeyW => Some(chip8_core::Key::new(0x5)),
        winit::keyboard::KeyCode::KeyE => Some(chip8_core::Key::new(0x6)),
        winit::keyboard::KeyCode::KeyR => Some(chip8_core::Key::new(0xD)),
        winit::keyboard::KeyCode::KeyA => Some(chip8_core::Key::new(0x7)),
        winit::keyboard::KeyCode::KeyS => Some(chip8_core::Key::new(0x8)),
        winit::keyboard::KeyCode::KeyD => Some(chip8_core::Key::new(0x9)),
        winit::keyboard::KeyCode::KeyF => Some(chip8_core::Key::new(0xE)),
        winit::keyboard::KeyCode::KeyZ => Some(chip8_core::Key::new(0xA)),
        winit::keyboard::KeyCode::KeyX => Some(chip8_core::Key::new(0x0)),
        winit::keyboard::KeyCode::KeyC => Some(chip8_core::Key::new(0xB)),
        winit::keyboard::KeyCode::KeyV => Some(chip8_core::Key::new(0xF)),
        _ => None,
    }
}

fn key_state_map(winit_key_state: winit::event::ElementState) -> chip8_core::KeyState {
    match winit_key_state {
        winit::event::ElementState::Pressed => chip8_core::KeyState::Pressed,
        winit::event::ElementState::Released => chip8_core::KeyState::Released,
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

struct BeepSound {
    sink: rodio::Sink,
    _stream: rodio::OutputStream,
}

impl BeepSound {
    fn new() -> Self {
        let stream = rodio::OutputStreamBuilder::open_default_stream().unwrap();
        let sink = rodio::Sink::connect_new(stream.mixer());
        let wave = rodio::source::SineWave::new(440.0);
        sink.append(wave);

        sink.set_volume(0.1);
        sink.pause();

        Self {
            sink,
            _stream: stream,
        }
    }

    fn on(&self) {
        self.sink.play();
    }

    fn off(&self) {
        self.sink.pause();
    }
}
