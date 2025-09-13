use std::path::Path;

use crate::{Address, Memory, ProgramCounter};

#[derive(Debug)]
pub struct Rom {
    bytes: Vec<u8>,
}

impl Rom {
    pub fn read_from_file(rom_file_path: impl AsRef<Path>) -> std::io::Result<Self> {
        let bytes = std::fs::read(rom_file_path)?;
        Ok(Self { bytes })
    }

    pub(crate) fn load_into_memory(&self, memory: &mut Memory, pc: &mut ProgramCounter) {
        memory[usize::from(PROGRAM_ADDR)..(usize::from(PROGRAM_ADDR) + self.bytes.len())]
            .copy_from_slice(&self.bytes);
        pc.jump_to_address(PROGRAM_ADDR);
    }
}

// CHIP-8 programs expect to be loaded at address 0x200 (512) due historical
// reasons (the first interpreters were located in RAM from 0x000 to 0x1FF).
const PROGRAM_ADDR: Address = 0x200;
