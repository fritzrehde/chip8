use crate::{Address, FRAME_HEIGHT, FRAME_WIDTH, Memory};

pub struct Framebuffer {
    pixels: [Pixel; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
    draw_status: DrawStatus,
}

pub enum DrawStatus {
    NeedsRedraw,
    Flushed,
}

impl Default for Framebuffer {
    fn default() -> Self {
        Self {
            pixels: [Pixel::Empty; (FRAME_WIDTH as usize) * (FRAME_HEIGHT as usize)],
            draw_status: DrawStatus::Flushed,
        }
    }
}

pub(crate) enum DrawSpriteResult {
    AnyPixelTurnedOff,
    NoPixelTurnedOff,
}

impl Framebuffer {
    pub(crate) fn draw_sprite(&mut self, sprite: Sprite, sprite_rows: &[u8]) -> DrawSpriteResult {
        let mut any_pixel_turned_off = false;

        for ((left_col_x, y), sprite_row) in
            steps_in_dir(sprite.top_left_x, sprite.top_left_y, Direction::South)
                .take(usize::from(sprite.height))
                .zip(sprite_rows)
        {
            for (pixel_idx, bit) in bits_msb_to_lsb(*sprite_row).enumerate().filter_map(
                |(offset_from_left_col, bit)| {
                    let x = left_col_x
                        + u8::try_from(offset_from_left_col)
                            .expect("a byte contains exactly 8 bits");
                    get_pixel_idx(x, y).map(|pixel_idx| (pixel_idx, bit))
                },
            ) {
                let prev_pixel = *self.get_pixel(pixel_idx);
                let new_pixel = match bit {
                    Bit::One => prev_pixel.flipped(),
                    Bit::Zero => prev_pixel,
                };
                self.set_pixel(pixel_idx, new_pixel);

                if prev_pixel == Pixel::Filled && new_pixel == Pixel::Empty {
                    any_pixel_turned_off = true;
                }
            }
        }

        if any_pixel_turned_off {
            DrawSpriteResult::AnyPixelTurnedOff
        } else {
            DrawSpriteResult::NoPixelTurnedOff
        }
    }

    pub(crate) fn clear(&mut self) {
        self.fill(Pixel::Empty);
    }

    fn fill(&mut self, pixel: Pixel) {
        self.pixels.fill(pixel);
        // If the framebuffer already contained only these pixels, a redraw is
        // unnecessary, but checking for that would probably not be much more
        // efficient than just redrawing.
        self.draw_status = DrawStatus::NeedsRedraw;
    }

    pub(crate) fn get_pixel(&mut self, pixel_idx: usize) -> &Pixel {
        &self.pixels[pixel_idx]
    }

    pub(crate) fn set_pixel(&mut self, pixel_idx: usize, pixel: Pixel) {
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

/// A sprite is a bitmap image, made up of pixels.
#[derive(Debug)]
pub(crate) struct Sprite {
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
    pub(crate) fn new(top_left_x: u8, top_left_y: u8, height: u8, bits_ptr: Address) -> Self {
        Self {
            top_left_x: top_left_x % FRAME_WIDTH,
            top_left_y: top_left_y % FRAME_HEIGHT,
            height,
            bits_ptr,
        }
    }

    /// Returns the rows of the sprite.
    pub(crate) fn read_rows_from_memory<'a>(&self, memory: &'a Memory) -> &'a [u8] {
        &memory[usize::from(self.bits_ptr)..(usize::from(self.bits_ptr) + usize::from(self.height))]
    }
}

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

pub(crate) fn get_pixel_idx(x_coord: u8, y_coord: u8) -> Option<usize> {
    let in_bounds = x_coord < FRAME_WIDTH && y_coord < FRAME_HEIGHT;
    if !in_bounds {
        return None;
    }
    Some((usize::from(y_coord) * usize::from(FRAME_WIDTH)) + usize::from(x_coord))
}
