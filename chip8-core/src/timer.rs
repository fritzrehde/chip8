pub(crate) struct Timer {
    time: u8,
}

impl Timer {
    pub(crate) fn new() -> Self {
        Self { time: 0 }
    }

    pub(crate) fn value(&self) -> u8 {
        self.time
    }

    pub(crate) fn set_value(&mut self, value: u8) {
        self.time = value;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TimerTickResult {
    Ticked,
    Inactive,
}

impl Timer {
    pub(crate) fn tick(&mut self) -> TimerTickResult {
        if self.time > 0 {
            self.time -= 1;
            TimerTickResult::Ticked
        } else {
            TimerTickResult::Inactive
        }
    }
}
