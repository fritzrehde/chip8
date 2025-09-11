use derive_new::new;

pub const DEFAULT_LETTERBOX_COLOUR: Colour = BLACK;
pub const DEFAULT_BACKGROUND_COLOUR: Colour = BLACK;
pub const DEFAULT_FOREGROUND_COLOUR: Colour = WHITE;

const BLACK: Colour = Colour {
    r: 0x00,
    g: 0x00,
    b: 0x00,
    a: 0xFF,
};
const WHITE: Colour = Colour {
    r: 0xFF,
    g: 0xFF,
    b: 0xFF,
    a: 0xFF,
};

#[derive(Debug, Copy, Clone)]
pub struct Colour {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

impl Colour {
    pub fn as_rgba(&self) -> [u8; 4] {
        [self.r, self.g, self.b, self.a]
    }
}

impl std::fmt::Display for Colour {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hex_color = hex_color::HexColor::from(self);
        write!(f, "{}", hex_color.display_rgba())?;
        Ok(())
    }
}

impl std::str::FromStr for Colour {
    type Err = hex_color::ParseHexColorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let hex_color = hex_color::HexColor::parse(s)?;
        let colour = Colour::from(hex_color);
        Ok(colour)
    }
}

impl From<hex_color::HexColor> for Colour {
    fn from(hex_color: hex_color::HexColor) -> Self {
        Self {
            r: hex_color.r,
            g: hex_color.g,
            b: hex_color.b,
            a: hex_color.a,
        }
    }
}

impl From<&Colour> for hex_color::HexColor {
    fn from(c: &Colour) -> Self {
        hex_color::HexColor {
            r: c.r,
            g: c.g,
            b: c.b,
            a: c.a,
        }
    }
}

impl From<Colour> for pixels::wgpu::Color {
    fn from(c: Colour) -> Self {
        let srgba = palette::Srgba::new(c.r, c.g, c.b, c.a);
        let linear = srgba.into_linear();
        pixels::wgpu::Color {
            r: linear.red,
            g: linear.green,
            b: linear.blue,
            a: linear.alpha,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct ColourPalette {
    pub foreground_colour: Colour,
    pub background_colour: Colour,
    pub letterbox_colour: Colour,
}
