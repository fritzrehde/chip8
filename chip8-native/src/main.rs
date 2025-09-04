use std::{
    cmp::min,
    path::PathBuf,
    thread,
    time::{Duration, Instant},
};

use chip8_core::{Cpu, Rom};
use clap::Parser;

#[derive(Debug, Parser)]
struct Cli {
    rom_file_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let rom = Rom::read_from_file(cli.rom_file_path)?;
    let mut cpu = Cpu::new();

    run_rom(&mut cpu, &rom);

    Ok(())
}

// TODO: make user-customizable
const INSTS_PER_SEC: f64 = 700.0;
const TICKS_PER_SEC: f64 = 60.0;

fn run_rom(cpu: &mut Cpu, rom: &Rom) {
    cpu.load_rom(rom);

    let time_between_insts = Duration::from_secs_f64(1.0 / INSTS_PER_SEC);
    let time_between_ticks = Duration::from_secs_f64(1.0 / TICKS_PER_SEC);

    let mut last = Instant::now();

    // Accumulators for how much time has passed since last tick/instruction.
    let mut tick_acc = Duration::ZERO;
    let mut inst_acc = Duration::ZERO;

    loop {
        // TODO: cap time since last to some max value to avoid long OS-related pauses causing catch-up bursts.
        let time_since_last = last.elapsed();
        last = Instant::now();
        tick_acc += time_since_last;
        inst_acc += time_since_last;

        // The reason for while loops rather than if stmts is that our interpreter
        // might miss deadlines (e.g. by being run on a slow host machine), so to
        // avoid skipping the tasks associated with the deadlines we missed, we
        // catch up on all of our accumulated debt.
        while tick_acc >= time_between_ticks {
            cpu.tick();
            tick_acc -= time_between_ticks;
        }
        while inst_acc >= time_between_insts {
            cpu.step();
            inst_acc -= time_between_insts;
        }

        // TODO: draw frame

        // TODO: maybe add a small buffer to ensure we sleep until just after the deadline.
        let time_to_next_tick_deadline = time_between_ticks - tick_acc;
        let time_to_next_inst_deadline = time_between_insts - inst_acc;
        let time_to_next_deadline = min(time_to_next_tick_deadline, time_to_next_inst_deadline);
        thread::sleep(time_to_next_deadline);
    }
}
