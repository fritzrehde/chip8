mod font;
mod framebuffer;
mod key;
mod memory;
mod rom;
mod timer;

use framebuffer::{DrawSpriteResult, Framebuffer, Sprite};
use memory::{Memory, ProgramCounter};
use rand::{Rng, rngs::ThreadRng};
use timer::{Timer, TimerTickResult};

pub use framebuffer::{DrawStatus, Pixel};
pub use key::{Key, KeyState};
pub use rom::Rom;

// 12-bit pointer addressing memory.
type Address = u16;
type RegisterId = u8;

pub const FRAME_WIDTH: u8 = 64;
pub const FRAME_HEIGHT: u8 = 32;

pub struct Cpu {
    state: CpuState,

    /// Program counter, points to current instruction in memory.
    pc: ProgramCounter,

    /// Growable stack of 12-bit values.
    stack: Vec<u16>,

    /// 4 KiB (4096 bytes) of RAM, addressable by 12 bits (2^12 = 4096),
    /// all of it writable.
    memory: Memory,

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
        let mut memory: Memory = Default::default();
        font::load_fontset_into_memory(&mut memory);

        Self {
            state: CpuState::Executing,
            pc: Default::default(),
            stack: Vec::new(),
            memory,
            framebuffer: Default::default(),
            index_register: 0x000,
            variable_registers: [0x0; 16],
            delay_timer: Timer::new(),
            sound_timer: Timer::new(),
            key_states: [KeyState::Released; 16],
            rng: rand::rng(),
        }
    }

    pub fn load_rom(&mut self, rom: &Rom) {
        rom.load_into_memory(&mut self.memory, &mut self.pc);
    }

    /// Fetch, decode and execute one instruction.
    pub fn step(&mut self) {
        let inst = self.fetch_next_inst();
        self.pc.increment();
        // Increment PC first to avoid skipping jumped-to instruction.
        self.exec_inst(inst);
    }

    pub(crate) fn draw_sprite(&mut self, sprite: Sprite) {
        let sprite_rows = sprite.read_rows_from_memory(&self.memory);
        self.variable_registers[0xF] = match self.framebuffer.draw_sprite(sprite, sprite_rows) {
            DrawSpriteResult::AnyPixelTurnedOff => 0x1,
            DrawSpriteResult::NoPixelTurnedOff => 0x0,
        };
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
        register_id: RegisterId,
        value: u8,
    },
    AddValueToVariableRegister {
        register_id: RegisterId,
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
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    BinaryOrVxVy {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    BinaryAndVxVy {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    LogicalXorVxVy {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    AddVyToVx {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    SubtractVyfromVx {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    SubtractVxfromVy {
        x_register_id: RegisterId,
        y_register_id: RegisterId,
    },
    ShiftLeft {
        x_register_id: RegisterId,
        _y_register_id: RegisterId,
    },
    ShiftRight {
        x_register_id: RegisterId,
        _y_register_id: RegisterId,
    },
    JumpWithOffset {
        address: Address,
        offset: u16,
    },
    Random {
        dst_register_id: RegisterId,
        bin_and_with_value: u8,
    },
    SetVxToDelayTimer {
        x_register_id: RegisterId,
    },
    SetDelayTimerToVx {
        x_register_id: RegisterId,
    },
    SetSoundTimerToVx {
        x_register_id: RegisterId,
    },
    SetIndexRegisterToFontAddress {
        font_char: u8,
    },
    SaveNumberAsDecimalDigits {
        number: u8,
    },
    StoreRegistersToMemory {
        upto_register_id: RegisterId,
    },
    LoadRegistersFromMemory {
        upto_register_id: RegisterId,
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
    next_pressed_key_dst_register_id: RegisterId,
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
            self.memory[usize::from(*self.pc)],
            self.memory[usize::from(*self.pc + 1)],
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
            Instruction::ClearScreen => self.framebuffer.clear(),
            Instruction::JumpTo { address } => self.pc.jump_to_address(address),
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
                self.pc.jump_to_address(address.wrapping_add(offset));
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
                self.index_register = font::get_font_addr(font_char);
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
                self.stack.push(*self.pc);
                self.pc.jump_to_address(subroutine_address);
            }
            Instruction::ReturnFromSubroutine => {
                let return_address = self
                    .stack
                    .pop()
                    .expect("stack should contain return address (PC) on return, but was empty");
                self.pc.jump_to_address(return_address);
            }
            Instruction::SkipInstructionIfValueEqual { value_a, value_b } => {
                if value_a == value_b {
                    self.pc.increment();
                }
            }
            Instruction::SkipInstructionIfValuesNotEqual { value_a, value_b } => {
                if value_a != value_b {
                    self.pc.increment();
                }
            }
            Instruction::WaitForInputKeyPress(wait_for_input_key_press) => {
                self.state = CpuState::WaitingForInput {
                    pending_instruction: wait_for_input_key_press,
                };
            }
            Instruction::SkipInstructionIfKeyPressed { key } => match self.key_states[key.idx()] {
                KeyState::Pressed => self.pc.increment(),
                KeyState::Released => {}
            },
            Instruction::SkipInstructionIfKeyNotPressed { key } => match self.key_states[key.idx()]
            {
                KeyState::Pressed => {}
                KeyState::Released => self.pc.increment(),
            },
        }
    }
}
