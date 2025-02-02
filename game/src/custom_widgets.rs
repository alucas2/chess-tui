use ratatui::{
    crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    layout::Rect,
    prelude::Buffer,
    style::{Color, Stylize},
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};

use crate::{clipboard, style};

use super::util;

/// Widget that represents an empty chessboard
#[derive(Clone, Copy)]
pub struct Board {
    pub rotated: bool,
}

/// Widget that represents a piece
#[derive(Clone, Copy)]
pub struct Piece {
    pub kind: engine::PieceKind,
    pub color: engine::PlayerColor,
}

/// Widget that represents a square
#[derive(Default, Clone, Copy)]
pub struct ColoredSquare {
    pub top_left_label: Option<char>,
    pub top_right_label: Option<char>,
    pub bottom_left_label: Option<char>,
    pub bottom_right_label: Option<char>,
    pub bg: Color,
    pub fg: Color,
    pub bg_blend_factor: f32,
}

/// Text area with custom key mapping
pub struct TextArea {
    pub inner: tui_textarea::TextArea<'static>,
}

/// Paragraph that wraps lines around span boundaries
pub struct WordWrappedParagraph<'a> {
    max_width: u16,
    lines: Vec<Line<'a>>,
}

/// Bottom bar that shows key mappings
#[derive(Default)]
pub struct BottomBar {
    text: Vec<Span<'static>>,
}

impl Board {
    pub const WIDTH: u16 = 8 * style::SQUARE_WIDTH;
    pub const HEIGHT: u16 = 8 * style::SQUARE_HEIGHT;

    /// Get the area occupied by a square, given the area occupied by the entire board
    pub fn inner(&self, square: engine::SquareIndex, outer: Rect) -> Rect {
        let (file, rank) = square.coords();
        let (square_x, square_y) = if self.rotated {
            (7 - file as u16, rank as u16)
        } else {
            (file as u16, 7 - rank as u16)
        };
        outer.intersection(Rect {
            x: outer.x + square_x * style::SQUARE_WIDTH,
            y: outer.y + square_y * style::SQUARE_HEIGHT,
            width: style::SQUARE_WIDTH,
            height: style::SQUARE_HEIGHT,
        })
    }
}

impl Widget for &Board {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        for square in engine::SquareIndex::iter() {
            let (file, rank) = square.coords();
            let (bg, fg) = if (file as u32 + rank as u32) % 2 == 0 {
                (style::color::DARK_SQUARE, style::color::LIGHT_SQUARE)
            } else {
                (style::color::LIGHT_SQUARE, style::color::DARK_SQUARE)
            };
            let rect = self.inner(square, area);
            ColoredSquare {
                bottom_left_label: (rect.bottom() == area.bottom()).then_some(file.label()),
                top_right_label: (rect.right() == area.right()).then_some(rank.label()),
                bg,
                fg,
                bg_blend_factor: 1.0,
                ..Default::default()
            }
            .render(rect, buf);
        }
    }
}

impl Widget for Piece {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let sprite = match self.kind {
            engine::PieceKind::Pawn => style::PAWN,
            engine::PieceKind::Knight => style::KNIGHT,
            engine::PieceKind::Bishop => style::BISHOP,
            engine::PieceKind::Rook => style::ROOK,
            engine::PieceKind::Queen => style::QUEEN,
            engine::PieceKind::King => style::KING,
        };
        let fill_color = match self.color {
            engine::PlayerColor::White => style::color::WHITE_PIECE,
            engine::PlayerColor::Black => style::color::BLACK_PIECE,
        };
        let color_lut = |id| match id {
            1 => Some(fill_color),
            2 => Some(util::blend(fill_color, Color::from_u32(0x303030), 0.5)),
            _ => None,
        };
        let rect = area.intersection(Rect {
            x: area.x,
            y: area.y,
            width: style::SQUARE_WIDTH,
            height: style::SQUARE_HEIGHT,
        });
        for (iy, row) in rect.rows().enumerate() {
            for (ix, p) in row.positions().enumerate() {
                let cell = &mut buf[p];
                let pixel_hi = color_lut(sprite[ix + 2 * iy * style::SQUARE_WIDTH as usize]);
                let pixel_lo = color_lut(sprite[ix + (2 * iy + 1) * style::SQUARE_WIDTH as usize]);
                match (pixel_hi, pixel_lo) {
                    (None, None) => continue,
                    (Some(x), None) => {
                        cell.set_char('▀');
                        cell.fg = x;
                    }
                    (None, Some(x)) => {
                        cell.set_char('▄');
                        cell.fg = x;
                    }
                    (Some(hi), Some(lo)) => {
                        cell.set_char('▄');
                        cell.bg = hi;
                        cell.fg = lo;
                    }
                }
            }
        }
    }
}

impl Widget for ColoredSquare {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let rect = area.intersection(Rect {
            x: area.x,
            y: area.y,
            width: style::SQUARE_WIDTH,
            height: style::SQUARE_HEIGHT,
        });
        for p in rect.positions() {
            let cell = &mut buf[p];
            if self.bg_blend_factor == 1.0 {
                cell.bg = self.bg
            } else {
                cell.bg = util::blend(cell.bg, self.bg, self.bg_blend_factor);
            }
        }
        for (c, p) in [
            self.top_left_label,
            self.top_right_label,
            self.bottom_left_label,
            self.bottom_right_label,
        ]
        .iter()
        .zip([
            (rect.left(), rect.top()),
            (rect.right() - 1, rect.top()),
            (rect.left(), rect.bottom() - 1),
            (rect.right() - 1, rect.bottom() - 1),
        ]) {
            if let (Some(c), Some(cell)) = (c, buf.cell_mut(p)) {
                cell.fg = self.fg;
                cell.set_char(*c);
            }
        }
    }
}

impl TextArea {
    pub fn new(init: String) -> TextArea {
        let mut inner = tui_textarea::TextArea::new(vec![init]);
        inner.move_cursor(tui_textarea::CursorMove::End);
        TextArea { inner }
    }

    pub fn text(&self) -> &str {
        &self.inner.lines()[0]
    }

    pub fn update(&mut self, event: Event) -> bool {
        let Event::Key(KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press | KeyEventKind::Repeat,
            ..
        }) = event
        else {
            return false;
        };
        if modifiers.contains(KeyModifiers::CONTROL) {
            match code {
                KeyCode::Char('a') => {
                    self.inner.select_all();
                    true
                }
                KeyCode::Char('c') => {
                    // Copy
                    self.inner.set_yank_text("");
                    self.inner.copy();
                    clipboard::set(self.inner.yank_text());
                    false
                }
                KeyCode::Char('v') => {
                    // Paste without newlines
                    let s = clipboard::get().lines().fold(String::new(), |a, b| a + b);
                    self.inner.set_yank_text(s);
                    self.inner.paste()
                }
                KeyCode::Char('x') => {
                    if self.inner.cut() {
                        clipboard::set(self.inner.yank_text());
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        } else {
            self.inner.input(tui_textarea::Input {
                key: tui_textarea::Key::from(code),
                ctrl: false,
                alt: false,
                shift: modifiers.contains(KeyModifiers::SHIFT),
            })
        }
    }
}

impl Widget for &TextArea {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        self.inner.render(area, buf);
    }
}

impl<'a> WordWrappedParagraph<'a> {
    pub fn new(max_width: u16) -> Self {
        WordWrappedParagraph {
            max_width,
            lines: vec![],
        }
    }

    pub fn push_word<T: Into<Span<'a>>>(&mut self, span: T) {
        let span = span.into();
        match self.lines.last_mut() {
            Some(last) if last.width() + 1 + span.width() <= self.max_width as usize => {
                last.push_span(" ");
                last.push_span(span)
            }
            _ => self.lines.push(span.into()),
        }
    }
}

impl Widget for WordWrappedParagraph<'_> {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        Paragraph::new(self.lines).render(area, buf)
    }
}

impl BottomBar {
    pub fn push(&mut self, key_label: &str, effect_label: &str) {
        self.text.extend([
            format!(" {key_label} ").reversed(),
            format!(" {effect_label} ").into(),
        ]);
    }
}

impl Widget for BottomBar {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        Line::from(self.text).render(area, buf)
    }
}
