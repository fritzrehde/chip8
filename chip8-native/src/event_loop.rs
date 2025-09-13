use std::{
    iter::zip,
    time::{Duration, Instant},
};

use chip8_core::{Cpu, CpuState, FRAME_HEIGHT, FRAME_WIDTH, Pixel, Rom};

use crate::{
    colour::{Colour, ColourPalette},
    display::Display,
    sound::BeepSound,
    utils::min_opt,
};

// Fixed tick rate, required by CHIP-8.
const TICKS_PER_SEC: f64 = 60.0;

pub(crate) struct App {
    cpu: Cpu,
    render: [Colour; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
    /// Window to display frame; only exists once "resumed" by OS, which only happens
    /// at startup for desktop apps (mobile apps can be suspended and resumed arbitrarily,
    /// but we only support desktop usage).
    display: Option<Display>,
    /// Deadline for the next CPU tick.
    next_tick: Instant,
    /// Deadline for executing the next instruction. Paused if CPU is waiting for input.
    next_inst: Option<Instant>,
    /// Deadline for requesting the next frame to be drawn. Paused if CPU is waiting for input.
    next_frame: Option<Instant>,
    time_between_ticks: Duration,
    time_between_insts: Duration,
    time_between_frames: Duration,
    colour_palette: ColourPalette,
    beep_sound: BeepSound,
}

impl App {
    pub(crate) fn new(
        mut cpu: Cpu,
        rom: &Rom,
        colour_palette: ColourPalette,
        instructions_per_sec: f64,
        frames_per_sec: f64,
    ) -> Self {
        cpu.load_rom(rom);

        let time_between_ticks = Duration::from_secs_f64(1.0 / TICKS_PER_SEC);
        let time_between_insts = Duration::from_secs_f64(1.0 / instructions_per_sec);
        let time_between_frames = Duration::from_secs_f64(1.0 / frames_per_sec);

        let now = Instant::now();

        Self {
            cpu,
            render: [colour_palette.background_colour;
                (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
            display: None,
            next_tick: now + time_between_ticks,
            next_inst: Some(now + time_between_insts),
            next_frame: Some(now + time_between_frames),
            time_between_ticks,
            time_between_insts,
            time_between_frames,
            colour_palette,
            beep_sound: BeepSound::new(),
        }
    }

    pub(crate) fn run(mut self) -> anyhow::Result<()> {
        let event_loop = winit::event_loop::EventLoop::new()?;
        event_loop.run_app(&mut self)?;
        Ok(())
    }

    // TODO: collect statistics on whether slowdowns are ever encountered (i.e. whether we ever need to catch up on missed deadlines by entering while loop body multiple times in one call)
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
        if let Some(next_inst) = &mut self.next_inst {
            let now = Instant::now();
            while now >= *next_inst {
                self.cpu.step();
                *next_inst += self.time_between_insts;
            }
            // The last executed instruction might have lead to a transition
            // from executing to waiting for input state.
            match self.cpu.state() {
                CpuState::Executing => {}
                CpuState::WaitingForInput { .. } => {
                    // While the cpu is blocked waiting for input, no instructions
                    // can be executed, which means the frame will also not
                    // be updated. CPU must continue to tick at all times, though.
                    self.next_inst = None;
                    self.next_frame = None;
                }
            }
        }
    }

    fn frame(&mut self) {
        if let Some(next_frame) = &mut self.next_frame {
            let now = Instant::now();
            let mut time_for_redraw = false;
            while now >= *next_frame {
                time_for_redraw = true;
                *next_frame += self.time_between_frames;
            }
            if time_for_redraw {
                // Optimisation: only redraw if framebuffer has been mutated since last draw.
                match self.cpu.framebuffer().draw_status() {
                    chip8_core::DrawStatus::NeedsRedraw => {
                        self.request_window_redraw();
                    }
                    chip8_core::DrawStatus::Flushed => {}
                };
            }
        }
    }

    fn request_window_redraw(&self) {
        let Some(display) = &self.display else {
            return;
        };
        display.request_redraw();
    }

    fn draw(&mut self) {
        let Some(display) = &mut self.display else {
            return;
        };
        // TODO: converting Pixels to u32 hex codes on every render is inefficient, think about maintaining duplicate u32 hex state alongside Pixel array, so we only pay when updating framebuffer (rare)
        let framebuffer = self.cpu.mut_framebuffer();
        for (rendered_pixel, pixel) in zip(self.render.iter_mut(), &*framebuffer) {
            *rendered_pixel = match pixel {
                Pixel::Filled => self.colour_palette.foreground_colour,
                Pixel::Empty => self.colour_palette.background_colour,
            };
        }
        display.render(&self.render);
        framebuffer.flush();
    }

    fn tick_and_step_and_frame(&mut self) {
        self.tick();
        self.step();
        self.frame();
    }

    fn next_deadline(&self) -> Instant {
        min_opt(self.next_inst, self.next_frame, self.next_tick)
    }

    fn schedule_next_timer_event(&self, event_loop: &winit::event_loop::ActiveEventLoop) {
        // Due to some unfortunate timings of OS scheduling, the deadline might be
        // in the past, but this is fine, as winit's eventloop will still execute
        // events scheduled for deadlines in the past.
        let deadline = self.next_deadline();
        event_loop.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(deadline));
    }
}

impl winit::application::ApplicationHandler for App {
    // On desktop, only called once at startup.
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.display = Some(Display::create(event_loop, &self.colour_palette));

        // Schedule the first task event, which then "piggy-back" off of each other.
        self.schedule_next_timer_event(event_loop);
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
                self.schedule_next_timer_event(event_loop);
            }
            winit::event::StartCause::WaitCancelled {
                start: _,
                requested_resume: _,
            } => {
                // We should just re-request the cancelled wait request, but we only
                // really care about our timer events, so just schedule that again directly.
                self.schedule_next_timer_event(event_loop);
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
        let Some(display) = &mut self.display else {
            return;
        };
        if display.window_id() != window_id {
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
                        // Unpause instruction- and frame-rates.
                        let now = Instant::now();
                        self.next_inst = Some(now + self.time_between_insts);
                        self.next_frame = Some(now + self.time_between_frames);
                    }
                    chip8_core::CpuStateChange::NoChange => {}
                };
            }
            winit::event::WindowEvent::Resized(size) => {
                if let Some(display) = &mut self.display {
                    display.resize(size.width, size.height);
                }
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

    let chip8_key_value = match winit_keycode {
        winit::keyboard::KeyCode::Digit1 => 0x1,
        winit::keyboard::KeyCode::Digit2 => 0x2,
        winit::keyboard::KeyCode::Digit3 => 0x3,
        winit::keyboard::KeyCode::Digit4 => 0xC,
        winit::keyboard::KeyCode::KeyQ => 0x4,
        winit::keyboard::KeyCode::KeyW => 0x5,
        winit::keyboard::KeyCode::KeyE => 0x6,
        winit::keyboard::KeyCode::KeyR => 0xD,
        winit::keyboard::KeyCode::KeyA => 0x7,
        winit::keyboard::KeyCode::KeyS => 0x8,
        winit::keyboard::KeyCode::KeyD => 0x9,
        winit::keyboard::KeyCode::KeyF => 0xE,
        winit::keyboard::KeyCode::KeyZ => 0xA,
        winit::keyboard::KeyCode::KeyX => 0x0,
        winit::keyboard::KeyCode::KeyC => 0xB,
        winit::keyboard::KeyCode::KeyV => 0xF,
        _ => return None,
    };
    Some(chip8_core::Key::new(chip8_key_value))
}

fn key_state_map(winit_key_state: winit::event::ElementState) -> chip8_core::KeyState {
    match winit_key_state {
        winit::event::ElementState::Pressed => chip8_core::KeyState::Pressed,
        winit::event::ElementState::Released => chip8_core::KeyState::Released,
    }
}

#[cfg(test)]
mod tests {
    #[cfg(target_os = "linux")]
    #[test]
    fn wait_until_past_deadline_wakes_immediately() {
        use std::sync::mpsc;
        use std::time::{Duration, Instant};
        use winit::application::ApplicationHandler;
        use winit::event::StartCause;
        use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
        use winit::platform::wayland::EventLoopBuilderExtWayland;

        struct TestApp {
            tx: mpsc::SyncSender<StartCause>,
        }

        impl ApplicationHandler for TestApp {
            fn new_events(&mut self, el: &ActiveEventLoop, cause: StartCause) {
                match cause {
                    StartCause::Init => {
                        // Schedule a deadline that is in the past.
                        el.set_control_flow(ControlFlow::WaitUntil(
                            Instant::now() - Duration::from_secs(1),
                        ));
                    }
                    // We expect this immediately after Init because the deadline is past.
                    StartCause::ResumeTimeReached { .. } => {
                        self.tx.send(cause).unwrap();
                        el.exit();
                    }
                    _ => {}
                }
            }

            fn resumed(&mut self, _event_loop: &winit::event_loop::ActiveEventLoop) {}

            fn window_event(
                &mut self,
                _event_loop: &winit::event_loop::ActiveEventLoop,
                _window_id: winit::window::WindowId,
                _event: winit::event::WindowEvent,
            ) {
            }
        }

        let (tx, rx) = mpsc::sync_channel(1);
        let mut app = TestApp { tx };
        let event_loop = EventLoop::builder().with_any_thread(true).build().unwrap();
        event_loop.run_app(&mut app).unwrap();

        let cause = rx
            .recv_timeout(Duration::from_secs(1))
            .expect("no msg received");

        assert!(matches!(cause, StartCause::ResumeTimeReached { .. }),);
    }
}
