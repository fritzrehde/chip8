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

    pub(crate) fn idx(&self) -> usize {
        usize::from(self.0)
    }
}

impl std::ops::Deref for Key {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
