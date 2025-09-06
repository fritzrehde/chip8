use std::path::Path;

use rand::{Rng, rngs::ThreadRng};

// 12-bit pointer addressing memory.
type Address = u16;

pub const FRAME_WIDTH: u8 = 64;
pub const FRAME_HEIGHT: u8 = 32;

pub struct Cpu {
    state: CpuState,

    /// Program counter, points to current instruction in memory.
    pc: Address,

    /// Growable stack of 12-bit values.
    stack: Vec<u16>,

    /// 4 KiB (4096 bytes) of RAM, addressable by 12 bits (2^12 = 4096),
    /// all of it writable.
    memory: [u8; 4096],

    /// The pixels that are displayed on the screen.
    framebuffer: Framebuffer,

    /// 12-bit "index"/"I" register, used to point at locations in memory.
    index_register: Address,

    /// 16 8-bit general-purpose "variable" registers.
    variable_registers: [u8; 16],

    /// While value is above 0, the delay timer should be decremented by one on
    /// every tick (e.g. at 60 Hz tick-rate). Also known as "DT".
    delay_timer: Timer,

    /// While value is above 0, the sound timer should be decremented by one on
    /// every tick (e.g. at 60 Hz tick-rate), and should "beep" audibly while
    /// above 0. Also known as "ST".
    sound_timer: Timer,

    /// The states of keys 0x0 to 0xF.
    key_states: [KeyState; 16],

    /// Random number generator.
    rng: ThreadRng,
}

#[derive(Debug, Copy, Clone)]
pub enum CpuState {
    Executing,
    WaitingForInput {
        pending_instruction: WaitForInputKeyPress,
    },
}

impl Cpu {
    pub fn new() -> Self {
        let mut s = Self {
            state: CpuState::Executing,
            pc: 0x000,
            stack: Vec::new(),
            memory: [0x0; 4096],
            framebuffer: Default::default(),
            index_register: 0x000,
            variable_registers: [0x0; 16],
            delay_timer: Timer::new(),
            sound_timer: Timer::new(),
            key_states: [KeyState::Released; 16],
            rng: rand::rng(),
        };
        s.load_fontset();
        s
    }

    fn load_fontset(&mut self) {
        // Load fontset.
        self.memory[usize::from(FONT_ADDR)..(usize::from(FONT_ADDR) + FONTSET.len())]
            .copy_from_slice(&FONTSET);
    }

    pub fn load_rom(&mut self, rom: &Rom) {
        self.memory[usize::from(PROGRAM_ADDR)..(usize::from(PROGRAM_ADDR) + rom.bytes.len())]
            .copy_from_slice(&rom.bytes);
        self.pc = PROGRAM_ADDR;
    }

    /// Fetch, decode and execute one instruction.
    pub fn step(&mut self) {
        let inst = self.fetch_next_inst();
        // PC indexes into byte array, but each instruction is 2 bytes wide.
        self.pc += 2;
        // Increment PC first to avoid skipping jumped-to instruction.
        self.exec_inst(inst);
    }

    fn draw_sprite(&mut self, sprite: Sprite) {
        let mut any_pixel_turned_off = false;

        for (i, (left_col_x, y)) in
            steps_in_dir(sprite.top_left_x, sprite.top_left_y, Direction::South)
                .take(usize::from(sprite.height))
                .enumerate()
        {
            let sprite_row = self.memory[usize::from(sprite.bits_ptr) + i];
            for (pixel_idx, _x, bit) in
                bits_msb_to_lsb(sprite_row)
                    .enumerate()
                    .filter_map(|(offset_from_left_col, bit)| {
                        let x = left_col_x
                            + u8::try_from(offset_from_left_col)
                                .expect("a byte contains exactly 8 bits");
                        get_pixel_idx(x, y).map(|pixel_idx| (pixel_idx, x, bit))
                    })
            {
                let prev_pixel = *self.framebuffer.get_pixel(pixel_idx);
                let new_pixel = match bit {
                    Bit::One => prev_pixel.flipped(),
                    Bit::Zero => prev_pixel,
                };
                self.framebuffer.set_pixel(pixel_idx, new_pixel);

                if prev_pixel == Pixel::Filled && new_pixel == Pixel::Empty {
                    any_pixel_turned_off = true;
                }
            }
        }

        self.variable_registers[0xF] = if any_pixel_turned_off { 0x1 } else { 0x0 };
    }

    pub fn framebuffer(&self) -> &Framebuffer {
        &self.framebuffer
    }

    pub fn mut_framebuffer(&mut self) -> &mut Framebuffer {
        &mut self.framebuffer
    }

    pub fn state(&self) -> &CpuState {
        &self.state
    }
}

pub enum ActionOnTick {
    PlayBeepSound,
    PauseBeepSound,
}

impl Cpu {
    /// A callback that will be called at the tick rate.
    pub fn tick(&mut self) -> ActionOnTick {
        self.delay_timer.tick();
        match self.sound_timer.tick() {
            TimerTickResult::Ticked => ActionOnTick::PlayBeepSound,
            TimerTickResult::Inactive => ActionOnTick::PauseBeepSound,
        }
    }
}

pub enum CpuStateChange {
    WaitingToExecuting,
    NoChange,
}

impl Cpu {
    /// Update a key to a new state.
    pub fn update_key_state(&mut self, key: Key, new_key_state: KeyState) -> CpuStateChange {
        self.key_states[key.idx()] = new_key_state;
        let (new_state, state_change) = match self.state {
            CpuState::Executing => (CpuState::Executing, CpuStateChange::NoChange),
            CpuState::WaitingForInput {
                pending_instruction,
            } => match new_key_state {
                KeyState::Pressed => {
                    pending_instruction.complete(&mut self.variable_registers, key);
                    (CpuState::Executing, CpuStateChange::WaitingToExecuting)
                }
                KeyState::Released => (
                    CpuState::WaitingForInput {
                        pending_instruction,
                    },
                    CpuStateChange::NoChange,
                ),
            },
        };
        self.state = new_state;
        state_change
    }
}

enum Instruction {
    ClearScreen,
    JumpTo {
        address: Address,
    },
    SetVariableRegisterToValue {
        register_id: u8,
        value: u8,
    },
    AddValueToVariableRegister {
        register_id: u8,
        value: u8,
    },
    SetIndexRegister {
        value: Address,
    },
    AddValueToIndexRegister {
        value: Address,
    },
    Draw(Sprite),
    SetVxToVy {
        x_register_id: u8,
        y_register_id: u8,
    },
    BinaryOrVxVy {
        x_register_id: u8,
        y_register_id: u8,
    },
    BinaryAndVxVy {
        x_register_id: u8,
        y_register_id: u8,
    },
    LogicalXorVxVy {
        x_register_id: u8,
        y_register_id: u8,
    },
    AddVyToVx {
        x_register_id: u8,
        y_register_id: u8,
    },
    SubtractVyfromVx {
        x_register_id: u8,
        y_register_id: u8,
    },
    SubtractVxfromVy {
        x_register_id: u8,
        y_register_id: u8,
    },
    ShiftLeft {
        x_register_id: u8,
        _y_register_id: u8,
    },
    ShiftRight {
        x_register_id: u8,
        _y_register_id: u8,
    },
    JumpWithOffset {
        address: Address,
        offset: u16,
    },
    Random {
        dst_register_id: u8,
        bin_and_with_value: u8,
    },
    SetVxToDelayTimer {
        x_register_id: u8,
    },
    SetDelayTimerToVx {
        x_register_id: u8,
    },
    SetSoundTimerToVx {
        x_register_id: u8,
    },
    SetIndexRegisterToFontAddress {
        font_char: u8,
    },
    SaveNumberAsDecimalDigits {
        number: u8,
    },
    StoreRegistersToMemory {
        upto_register_id: u8,
    },
    LoadRegistersFromMemory {
        upto_register_id: u8,
    },
    CallSubroutine {
        subroutine_address: Address,
    },
    ReturnFromSubroutine,
    SkipInstructionIfValueEqual {
        value_a: u8,
        value_b: u8,
    },
    SkipInstructionIfValuesNotEqual {
        value_a: u8,
        value_b: u8,
    },
    WaitForInputKeyPress(WaitForInputKeyPress),
    SkipInstructionIfKeyPressed {
        key: Key,
    },
    SkipInstructionIfKeyNotPressed {
        key: Key,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct WaitForInputKeyPress {
    /// Once any key is pressed, save the pressed key in this register.
    next_pressed_key_dst_register_id: u8,
}

impl WaitForInputKeyPress {
    fn complete(&self, variable_registers: &mut [u8; 16], key: Key) {
        // Complete the instruction that caused the wait.
        variable_registers[usize::from(self.next_pressed_key_dst_register_id)] = *key;
    }
}

impl Cpu {
    fn fetch_next_inst(&self) -> Instruction {
        // Read the instruction that PC is currently pointing at from memory.
        let (opcode_high_nibble, opcode_low_nibble) = (
            self.memory[usize::from(self.pc)],
            self.memory[usize::from(self.pc + 1)],
        );

        // opcode: [ F | X | Y | N ]
        //         [ _ | _ | NN    ]
        //         [ _ | NNN       ]
        let f = (opcode_high_nibble & 0xF0) >> 4;
        let x = opcode_high_nibble & 0x0F;
        let y = (opcode_low_nibble & 0xF0) >> 4;
        let n = opcode_low_nibble & 0x0F;
        let nn = opcode_low_nibble;
        let nnn: Address =
            ((u16::from(opcode_high_nibble) & 0x0F) << 8) | u16::from(opcode_low_nibble);

        let vx = self.variable_registers[usize::from(x)];
        let vy = self.variable_registers[usize::from(y)];

        match (f, x, y, n) {
            (0x0, 0x0, 0xE, 0x0) => Instruction::ClearScreen,
            (0x1, _, _, _) => Instruction::JumpTo { address: nnn },
            (0x6, _, _, _) => Instruction::SetVariableRegisterToValue {
                register_id: x,
                value: nn,
            },
            (0x7, _, _, _) => Instruction::AddValueToVariableRegister {
                register_id: x,
                value: nn,
            },
            (0xA, _, _, _) => Instruction::SetIndexRegister { value: nnn },
            (0xD, _, _, _) => Instruction::Draw(Sprite::new(vx, vy, n, self.index_register)),
            (0x8, _, _, 0x0) => Instruction::SetVxToVy {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x1) => Instruction::BinaryOrVxVy {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x2) => Instruction::BinaryAndVxVy {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x3) => Instruction::LogicalXorVxVy {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x4) => Instruction::AddVyToVx {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x5) => Instruction::SubtractVyfromVx {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x7) => Instruction::SubtractVxfromVy {
                x_register_id: x,
                y_register_id: y,
            },
            (0x8, _, _, 0x6) => Instruction::ShiftRight {
                x_register_id: x,
                _y_register_id: y,
            },
            (0x8, _, _, 0xE) => Instruction::ShiftLeft {
                x_register_id: x,
                _y_register_id: y,
            },
            (0xB, _, _, _) => Instruction::JumpWithOffset {
                address: nnn,
                offset: u16::from(self.variable_registers[0x0]),
            },
            (0xC, _, _, _) => Instruction::Random {
                dst_register_id: x,
                bin_and_with_value: nn,
            },
            (0xF, _, 0x0, 0x7) => Instruction::SetVxToDelayTimer { x_register_id: x },
            (0xF, _, 0x1, 0x5) => Instruction::SetDelayTimerToVx { x_register_id: x },
            (0xF, _, 0x1, 0x8) => Instruction::SetSoundTimerToVx { x_register_id: x },
            (0xF, _, 0x1, 0xE) => Instruction::AddValueToIndexRegister {
                value: u16::from(vx),
            },
            (0xF, _, 0x2, 0x9) => Instruction::SetIndexRegisterToFontAddress {
                font_char: vx & 0xF,
            },
            (0xF, _, 0x3, 0x3) => Instruction::SaveNumberAsDecimalDigits { number: vx },
            (0xF, _, 0x5, 0x5) => Instruction::StoreRegistersToMemory {
                upto_register_id: x,
            },
            (0xF, _, 0x6, 0x5) => Instruction::LoadRegistersFromMemory {
                upto_register_id: x,
            },
            (0x2, _, _, _) => Instruction::CallSubroutine {
                subroutine_address: nnn,
            },
            (0x0, 0x0, 0xE, 0xE) => Instruction::ReturnFromSubroutine,
            (0x3, _, _, _) => Instruction::SkipInstructionIfValueEqual {
                value_a: vx,
                value_b: nn,
            },
            (0x4, _, _, _) => Instruction::SkipInstructionIfValuesNotEqual {
                value_a: vx,
                value_b: nn,
            },
            (0x5, _, _, 0x0) => Instruction::SkipInstructionIfValueEqual {
                value_a: vx,
                value_b: vy,
            },
            (0x9, _, _, 0x0) => Instruction::SkipInstructionIfValuesNotEqual {
                value_a: vx,
                value_b: vy,
            },
            (0xF, _, 0x0, 0xA) => Instruction::WaitForInputKeyPress(WaitForInputKeyPress {
                next_pressed_key_dst_register_id: x,
            }),
            (0xE, _, 0x9, 0xE) => Instruction::SkipInstructionIfKeyPressed { key: Key::new(vx) },
            (0xE, _, 0xA, 0x1) => Instruction::SkipInstructionIfKeyNotPressed { key: Key::new(vx) },
            _ => todo!(),
        }
    }

    fn exec_inst(&mut self, inst: Instruction) {
        match inst {
            Instruction::ClearScreen => self.framebuffer.fill(Pixel::Empty),
            Instruction::JumpTo { address } => self.pc = address,
            Instruction::SetVariableRegisterToValue { register_id, value } => {
                self.variable_registers[usize::from(register_id)] = value;
            }
            Instruction::AddValueToVariableRegister { register_id, value } => {
                self.variable_registers[usize::from(register_id)] =
                    self.variable_registers[usize::from(register_id)].wrapping_add(value);
            }
            Instruction::SetIndexRegister { value } => self.index_register = value,
            Instruction::Draw(sprite) => self.draw_sprite(sprite),
            Instruction::SetVxToVy {
                x_register_id,
                y_register_id,
            } => {
                self.variable_registers[usize::from(x_register_id)] =
                    self.variable_registers[usize::from(y_register_id)]
            }
            Instruction::BinaryOrVxVy {
                x_register_id,
                y_register_id,
            } => {
                self.variable_registers[usize::from(x_register_id)] |=
                    self.variable_registers[usize::from(y_register_id)]
            }
            Instruction::BinaryAndVxVy {
                x_register_id,
                y_register_id,
            } => {
                self.variable_registers[usize::from(x_register_id)] &=
                    self.variable_registers[usize::from(y_register_id)]
            }
            Instruction::LogicalXorVxVy {
                x_register_id,
                y_register_id,
            } => {
                self.variable_registers[usize::from(x_register_id)] ^=
                    self.variable_registers[usize::from(y_register_id)]
            }
            Instruction::AddVyToVx {
                x_register_id,
                y_register_id,
            } => {
                let vx = self.variable_registers[usize::from(x_register_id)];
                let vy = self.variable_registers[usize::from(y_register_id)];

                let (sum, overflowed) = vx.overflowing_add(vy);
                self.variable_registers[usize::from(x_register_id)] = sum;
                self.variable_registers[0xF] = if overflowed { 0x1 } else { 0x0 };
            }
            Instruction::SubtractVyfromVx {
                x_register_id,
                y_register_id,
            } => {
                let vx = self.variable_registers[usize::from(x_register_id)];
                let vy = self.variable_registers[usize::from(y_register_id)];

                let (diff, underflowed) = vx.overflowing_sub(vy);
                self.variable_registers[usize::from(x_register_id)] = diff;
                self.variable_registers[0xF] = if underflowed { 0x0 } else { 0x1 };
            }
            Instruction::SubtractVxfromVy {
                x_register_id,
                y_register_id,
            } => {
                let vx = self.variable_registers[usize::from(x_register_id)];
                let vy = self.variable_registers[usize::from(y_register_id)];

                let (diff, underflowed) = vy.overflowing_sub(vx);
                self.variable_registers[usize::from(x_register_id)] = diff;
                self.variable_registers[0xF] = if underflowed { 0x0 } else { 0x1 };
            }
            Instruction::ShiftRight {
                x_register_id,
                _y_register_id: _,
            } => {
                // TODO: setting vx to vy was removed by later CHIP impls, so make user-customisable.
                // let vy = self.variable_registers[usize::from(y_register_id)];
                // self.variable_registers[usize::from(x_register_id)] = vy;

                let shifted_out_bit =
                    self.variable_registers[usize::from(x_register_id)] & 0b0000_0001;
                self.variable_registers[usize::from(x_register_id)] >>= 1;
                self.variable_registers[0xF] = shifted_out_bit;
            }
            Instruction::ShiftLeft {
                x_register_id,
                _y_register_id: _,
            } => {
                // TODO: setting vx to vy was removed by later CHIP impls, so make user-customisable.
                // let vy = self.variable_registers[usize::from(y_register_id)];
                // self.variable_registers[usize::from(x_register_id)] = vy;

                let shifted_out_bit =
                    (self.variable_registers[usize::from(x_register_id)] & 0b1000_0000) >> 7;
                self.variable_registers[usize::from(x_register_id)] <<= 1;
                self.variable_registers[0xF] = shifted_out_bit;
            }
            Instruction::JumpWithOffset { address, offset } => {
                self.pc = address.wrapping_add(offset)
            }
            Instruction::Random {
                dst_register_id,
                bin_and_with_value,
            } => {
                let randon_num: u8 = self.rng.random();
                self.variable_registers[usize::from(dst_register_id)] =
                    randon_num & bin_and_with_value;
            }
            Instruction::SetVxToDelayTimer { x_register_id } => {
                self.variable_registers[usize::from(x_register_id)] = self.delay_timer.value();
            }
            Instruction::SetDelayTimerToVx { x_register_id } => {
                let vx = self.variable_registers[usize::from(x_register_id)];
                self.delay_timer.set_value(vx);
            }
            Instruction::SetSoundTimerToVx { x_register_id } => {
                let vx = self.variable_registers[usize::from(x_register_id)];
                self.sound_timer.set_value(vx);
            }
            Instruction::AddValueToIndexRegister { value } => {
                self.index_register = self.index_register.wrapping_add(value);
            }
            Instruction::SetIndexRegisterToFontAddress { font_char } => {
                self.index_register = get_font_addr(font_char);
            }
            Instruction::SaveNumberAsDecimalDigits { number } => {
                // Number is between 0 and 255.
                // Dividing by 10 in decimal is equivalent to shifting one to the right in binary.
                // Modulo 10 in decimal is equivalent to masking out lowest digit.
                let digit1 = number / 100;
                let digit2 = (number / 10) % 10;
                let digit3 = number % 10;
                self.memory[usize::from(self.index_register)] = digit1;
                self.memory[usize::from(self.index_register) + 1] = digit2;
                self.memory[usize::from(self.index_register) + 2] = digit3;
            }
            Instruction::StoreRegistersToMemory { upto_register_id } => {
                // TODO: some versions of chip8 incremented index register in loop, make this optional behaviour.
                for (i, register_id) in (0..=upto_register_id).enumerate() {
                    self.memory[usize::from(self.index_register) + i] =
                        self.variable_registers[usize::from(register_id)];
                }
            }
            Instruction::LoadRegistersFromMemory { upto_register_id } => {
                // TODO: some versions of chip8 incremented index register in loop, make this optional behaviour.
                for (i, register_id) in (0..=upto_register_id).enumerate() {
                    self.variable_registers[usize::from(register_id)] =
                        self.memory[usize::from(self.index_register) + i];
                }
            }
            Instruction::CallSubroutine { subroutine_address } => {
                // Save the PC to the stack for us to retrieve it when returning
                // after the subroutine call completes.
                self.stack.push(self.pc);
                self.pc = subroutine_address;
            }
            Instruction::ReturnFromSubroutine => {
                let return_address = self
                    .stack
                    .pop()
                    .expect("stack should contain return address (PC) on return, but was empty");
                self.pc = return_address;
            }
            Instruction::SkipInstructionIfValueEqual { value_a, value_b } => {
                if value_a == value_b {
                    self.pc += 2;
                }
            }
            Instruction::SkipInstructionIfValuesNotEqual { value_a, value_b } => {
                if value_a != value_b {
                    self.pc += 2;
                }
            }
            Instruction::WaitForInputKeyPress(wait_for_input_key_press) => {
                self.state = CpuState::WaitingForInput {
                    pending_instruction: wait_for_input_key_press,
                };
            }
            Instruction::SkipInstructionIfKeyPressed { key } => match self.key_states[key.idx()] {
                KeyState::Pressed => self.pc += 2,
                KeyState::Released => {}
            },
            Instruction::SkipInstructionIfKeyNotPressed { key } => match self.key_states[key.idx()]
            {
                KeyState::Pressed => {}
                KeyState::Released => self.pc += 2,
            },
        }
    }
}

#[derive(Debug)]
pub struct Rom {
    bytes: Vec<u8>,
}

impl Rom {
    pub fn read_from_file(rom_file_path: impl AsRef<Path>) -> std::io::Result<Self> {
        let bytes = std::fs::read(rom_file_path)?;
        Ok(Self { bytes })
    }
}

pub enum DrawStatus {
    NeedsRedraw,
    Flushed,
}

pub struct Framebuffer {
    pixels: [Pixel; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
    draw_status: DrawStatus,
}

impl Default for Framebuffer {
    fn default() -> Self {
        Self {
            pixels: [Pixel::Empty; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
            draw_status: DrawStatus::Flushed,
        }
    }
}

impl Framebuffer {
    fn fill(&mut self, pixel: Pixel) {
        self.pixels.fill(pixel);
        // If the framebuffer already contained only these pixels, a redraw is
        // unnecessary, but checking for that would probably not be much more
        // efficient than just redrawing.
        self.draw_status = DrawStatus::NeedsRedraw;
    }

    fn get_pixel(&mut self, pixel_idx: usize) -> &Pixel {
        &self.pixels[pixel_idx]
    }

    fn set_pixel(&mut self, pixel_idx: usize, pixel: Pixel) {
        self.pixels[pixel_idx] = pixel;
        self.draw_status = DrawStatus::NeedsRedraw;
    }

    pub fn draw_status(&self) -> &DrawStatus {
        &self.draw_status
    }

    /// Should be called if the framebuffer has been drawn/flushed to the graphics layer.
    pub fn flush(&mut self) {
        self.draw_status = DrawStatus::Flushed;
    }
}

impl std::fmt::Debug for Framebuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..FRAME_HEIGHT {
            for x in 0..FRAME_WIDTH {
                write!(
                    f,
                    "{}",
                    self.pixels[get_pixel_idx(x, y).expect("x and y are in range")]
                )?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'a> IntoIterator for &'a Framebuffer {
    type Item = &'a Pixel;
    type IntoIter = std::slice::Iter<'a, Pixel>;

    fn into_iter(self) -> Self::IntoIter {
        self.pixels.iter()
    }
}

#[derive(Debug, Copy, Clone)]
enum Bit {
    Zero,
    One,
}

fn bits_msb_to_lsb(byte: u8) -> impl Iterator<Item = Bit> {
    (0u8..8).rev().map(move |i| match (byte & (1 << i)) >> i {
        0 => Bit::Zero,
        1 => Bit::One,
        _ => unreachable!(),
    })
}

enum Direction {
    _North,
    _East,
    South,
    _West,
}

impl Direction {
    fn xy_diff(&self) -> (i8, i8) {
        // Origin (0,0) is top left.
        match self {
            Direction::_North => (0, -1),
            Direction::_East => (1, 0),
            Direction::South => (0, 1),
            Direction::_West => (-1, 0),
        }
    }
}

fn steps_in_dir(x_start: u8, y_start: u8, dir: Direction) -> impl Iterator<Item = (u8, u8)> {
    let (mut x, mut y) = (i16::from(x_start), i16::from(y_start));
    let (x_diff, y_diff) = dir.xy_diff();
    std::iter::from_fn(move || {
        let in_bounds =
            (0 <= x && x < i16::from(FRAME_WIDTH)) && (0 <= y && y < i16::from(FRAME_HEIGHT));
        if !in_bounds {
            return None;
        }
        let (old_x, old_y) = (
            u8::try_from(x).expect("in bounds is definitely u8"),
            u8::try_from(y).expect("in bounds is definitely u8"),
        );
        x += i16::from(x_diff);
        y += i16::from(y_diff);
        Some((old_x, old_y))
    })
}

/// A sprite is a square bitmap image, made up of pixels.
#[derive(Debug)]
struct Sprite {
    /// x-coordinate of top left of the sprite.
    top_left_x: u8,
    /// y-coordinate of top left of the sprite.
    top_left_y: u8,
    /// Height of the sprite. The width of a sprite is always represented by
    /// 1 byte, so is made up of 8 bits/pixels.
    height: u8,
    /// Pointer to the memory location storing the bits of the sprite in row-wise order.
    bits_ptr: Address,
}

impl Sprite {
    fn new(top_left_x: u8, top_left_y: u8, height: u8, bits_ptr: Address) -> Self {
        Self {
            top_left_x: top_left_x % FRAME_WIDTH,
            top_left_y: top_left_y % FRAME_HEIGHT,
            height,
            bits_ptr,
        }
    }
}

const BYTES_PER_FONT_CHAR: u8 = 5;

/// Each char from 0x0 to 0xF is represented by a bitmap made up of 5 bytes,
/// where the top 4 bits of each byte represent one row of the char.
const FONTSET: [u8; 16 * (BYTES_PER_FONT_CHAR as usize)] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

/// The address in memory where the fontset is loaded.
const FONT_ADDR: Address = 0x050;

/// Get address of a font character in memory.
fn get_font_addr(font_char: u8) -> Address {
    FONT_ADDR + u16::from(font_char) * u16::from(BYTES_PER_FONT_CHAR)
}

// CHIP-8 programs expect to be loaded at address 0x200 (512) due historical
// reasons (the first interpreters were located in RAM from 0x000 to 0x1FF).
const PROGRAM_ADDR: Address = 0x200;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pixel {
    Filled,
    Empty,
}

impl Pixel {
    fn flipped(&self) -> Pixel {
        match self {
            Pixel::Filled => Pixel::Empty,
            Pixel::Empty => Pixel::Filled,
        }
    }
}

impl std::fmt::Display for Pixel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bit = match self {
            Pixel::Filled => 1,
            Pixel::Empty => 0,
        };
        write!(f, "{}", bit)?;
        Ok(())
    }
}

fn get_pixel_idx(x_coord: u8, y_coord: u8) -> Option<usize> {
    let in_bounds = x_coord < FRAME_WIDTH && y_coord < FRAME_HEIGHT;
    if !in_bounds {
        return None;
    }
    Some((usize::from(y_coord) * usize::from(FRAME_WIDTH)) + usize::from(x_coord))
}

struct Timer {
    time: u8,
}

impl Timer {
    fn new() -> Self {
        Self { time: 0 }
    }

    fn value(&self) -> u8 {
        self.time
    }

    fn set_value(&mut self, value: u8) {
        self.time = value;
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TimerTickResult {
    Ticked,
    Inactive,
}

impl Timer {
    fn tick(&mut self) -> TimerTickResult {
        if self.time > 0 {
            self.time -= 1;
            TimerTickResult::Ticked
        } else {
            TimerTickResult::Inactive
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum KeyState {
    Pressed,
    Released,
}

#[derive(Debug, Copy, Clone)]
pub struct Key(u8);

impl Key {
    pub fn new(key_value: u8) -> Self {
        assert!(key_value <= 0xF);
        Self(key_value)
    }

    fn idx(&self) -> usize {
        usize::from(self.0)
    }
}

impl std::ops::Deref for Key {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
