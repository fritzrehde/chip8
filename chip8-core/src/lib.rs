use num_traits::{PrimInt, Unsigned};

// 12-bit pointer addressing memory.
type Address = u16;

struct Cpu {
    /// Program counter, points to current instruction in memory.
    pc: Address,

    /// Stack pointer (assume stack will not grow beyond size of memory).
    sp: Address,

    /// Growable stack of 12-bit values.
    stack: Vec<u16>,

    /// 4 KiB (4096 bytes) of RAM, addressable by 12 bits (2^12 = 4096),
    /// all of it writable.
    memory: [u8; 4096],

    /// 12-bit "index"/"I" register, used to point at locations in memory.
    index_register: Address,

    /// 16 8-bit general-purpose "variable" registers.
    variable_registers: [u8; 16],

    /// While value is above 0, the delay timer should be decremented by one
    /// 60 times per second (ie. at 60 Hz). Also known as "DT".
    delay_timer: Timer,

    /// While value is above 0, the sound timer should be decremented by one
    /// 60 times per second (ie. at 60 Hz), and should "beep" audibly while
    /// above 0. Also known as "ST".
    sound_timer: Timer,
}

impl Cpu {
    fn new() -> Self {
        Self {
            pc: 0x000,
            sp: 0x000,
            stack: Vec::new(),
            memory: [0x0; 4096],
            index_register: 0x000,
            variable_registers: [0x0; 16],
            delay_timer: Timer::new(),
            sound_timer: Timer::new(),
        }
    }

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
            ((u16::from(opcode_high_nibble) & 0x0F) << 8) & u16::from(opcode_low_nibble);

        let vx = self.variable_registers[usize::from(x)];
        let vy = self.variable_registers[usize::from(y)];

        match (f, x, y, n) {
            (0x0, 0x0, 0xE, 0x0) => Instruction::ClearScreen,
            (0x1, _, _, _) => Instruction::JumpTo { address: nnn },
            (0x6, _, _, _) => Instruction::SetVariableRegister {
                register_id: x,
                value: nn,
            },
            (0x7, _, _, _) => Instruction::AddToVariableRegister {
                register_id: x,
                value: nn,
            },
            (0xA, _, _, _) => Instruction::SetIndexRegister { value: nnn },
            (0xD, _, _, _) => Instruction::Display { x: vx, y: vy, n },
            _ => todo!(),
        }
    }

    fn exec_inst(&mut self, inst: Instruction) {
        match inst {
            Instruction::ClearScreen => todo!(),
            Instruction::JumpTo { address } => self.pc = address,
            Instruction::SetVariableRegister { register_id, value } => {
                self.variable_registers[usize::from(register_id)] = value;
            }
            Instruction::AddToVariableRegister { register_id, value } => {
                self.variable_registers[usize::from(register_id)] += value;
            }
            Instruction::SetIndexRegister { value } => self.index_register = value,
            Instruction::Display { x, y, n } => todo!(),
        }
    }

    // TODO: should be called at approx 700 instructions per sec.
    /// Fetch, decode and execute one instruction.
    fn step(&mut self) {
        let inst = self.fetch_next_inst();
        self.exec_inst(inst);
        // PC indexes into byte array, but each instruction is 2 bytes wide.
        self.pc += 2;
    }

    /// A callback that will be called at a 60hz rate.
    fn tick_60hz(&mut self) {
        self.delay_timer.tick();
        if self.sound_timer.tick() == TimerTickResult::Ticked {
            // TODO: play sound
        }
    }
}

enum Instruction {
    ClearScreen,
    JumpTo { address: Address },
    SetVariableRegister { register_id: u8, value: u8 },
    AddToVariableRegister { register_id: u8, value: u8 },
    SetIndexRegister { value: Address },
    Display { x: u8, y: u8, n: u8 },
}

// CHIP-8 programs expect to be loaded at address 0x200 (512) due historical
// reasons (the first interpreters were located in RAM from 0x000 to 0x1FF).
const PROGRAM_LOAD_OFFSET: u32 = 0x200;

struct Timer {
    time: u8,
}

impl Timer {
    fn new() -> Self {
        Self { time: 0 }
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
