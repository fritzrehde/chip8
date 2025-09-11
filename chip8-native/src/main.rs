mod colour;
mod display;
mod event_loop;
mod sound;
mod utils;

use std::path::PathBuf;

use chip8_core::{Cpu, Rom};
use clap::Parser;
use colour::{
    Colour, ColourPalette, DEFAULT_BACKGROUND_COLOUR, DEFAULT_FOREGROUND_COLOUR,
    DEFAULT_LETTERBOX_COLOUR,
};
use event_loop::App;

const DEFAULT_INSTRUCTIONS_PER_SEC: f64 = 700.0;
const DEFAULT_FRAMES_PER_SEC: f64 = 60.0;

#[derive(Debug, Parser)]
struct Cli {
    /// Path to the ROM file to be played.
    #[arg(short = 'r', long = "rom", value_name = "PATH", required = true)]
    rom_file_path: PathBuf,

    /// Colour used in background of the game.
    #[arg(long = "background-colour", value_name = "COLOR", default_value_t = DEFAULT_BACKGROUND_COLOUR)]
    background_colour: Colour,

    /// Colour used in foreground of the game.
    #[arg(long = "foreground-colour", value_name = "COLOR", default_value_t = DEFAULT_FOREGROUND_COLOUR)]
    foreground_colour: Colour,

    /// Colour used in window space outside of game (letterboxing).
    #[arg(long = "letterbox-colour", value_name = "COLOR", default_value_t = DEFAULT_LETTERBOX_COLOUR)]
    letterbox_colour: Colour,

    /// Instructions per second.
    #[arg(long = "insts-per-sec", value_name = "f64", default_value_t = DEFAULT_INSTRUCTIONS_PER_SEC)]
    instructions_per_sec: f64,

    /// Frames per second.
    #[arg(long = "frames-per-sec", value_name = "f64", default_value_t = DEFAULT_FRAMES_PER_SEC)]
    frames_per_sec: f64,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let rom = Rom::read_from_file(cli.rom_file_path)?;
    let cpu = Cpu::new();
    let colour_palette = ColourPalette::new(
        cli.foreground_colour,
        cli.background_colour,
        cli.letterbox_colour,
    );

    let app = App::new(
        cpu,
        &rom,
        colour_palette,
        cli.instructions_per_sec,
        cli.frames_per_sec,
    );
    app.run()?;

    Ok(())
}
