use crate::Address;

pub(crate) struct Memory {
    bytes: [u8; 4096],
}

impl Default for Memory {
    fn default() -> Self {
        Self { bytes: [0x0; 4096] }
    }
}

impl<I> std::ops::Index<I> for Memory
where
    [u8]: std::ops::Index<I>,
{
    type Output = <[u8] as std::ops::Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.bytes[index]
    }
}

impl<I> std::ops::IndexMut<I> for Memory
where
    [u8]: std::ops::IndexMut<I>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.bytes[index]
    }
}

pub(crate) struct ProgramCounter {
    pc: Address,
}

impl ProgramCounter {
    pub(crate) fn jump_to_address(&mut self, address: Address) {
        self.pc = address;
    }

    pub(crate) fn increment(&mut self) {
        // PC indexes into byte array, and each instruction is 2 bytes wide.
        self.pc += 2;
    }
}

impl Default for ProgramCounter {
    fn default() -> Self {
        Self { pc: 0x000 }
    }
}

impl std::ops::Deref for ProgramCounter {
    type Target = Address;

    fn deref(&self) -> &Self::Target {
        &self.pc
    }
}
