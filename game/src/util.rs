use ratatui::{
    crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind},
    layout::{Constraint, Layout, Rect},
    style::Color,
};

pub fn to_keycode(event: Event) -> Option<KeyCode> {
    match event {
        Event::Key(KeyEvent {
            code,
            kind: KeyEventKind::Press | KeyEventKind::Repeat,
            ..
        }) => Some(code),
        _ => None,
    }
}

pub fn layout_centered(width: u16, height: u16, outer_area: Rect) -> Rect {
    let [_, result, _] = Layout::vertical([
        Constraint::Fill(1),
        Constraint::Length(height),
        Constraint::Fill(1),
    ])
    .areas(outer_area);
    let [_, result, _] = Layout::horizontal([
        Constraint::Fill(1),
        Constraint::Length(width),
        Constraint::Fill(1),
    ])
    .areas(result);
    result
}

pub fn blend(c1: Color, c2: Color, c2_factor: f32) -> Color {
    let c1_factor = 1.0 - c2_factor;
    let blend_component = |k1, k2| {
        let k1 = k1 as f32 / 255.0;
        let k2 = k2 as f32 / 255.0;
        ((k1 * c1_factor + k2 * c2_factor).clamp(0.0, 1.0) * 255.0) as u8
    };
    match (c1, c2) {
        (Color::Rgb(r1, g1, b1), Color::Rgb(r2, g2, b2)) => Color::Rgb(
            blend_component(r1, r2),
            blend_component(g1, g2),
            blend_component(b1, b2),
        ),
        _ => c1,
    }
}

pub fn file_index_clamp(index: i8) -> engine::FileIndex {
    match index.clamp(0, 7) {
        0 => engine::FileIndex::A,
        1 => engine::FileIndex::B,
        2 => engine::FileIndex::C,
        3 => engine::FileIndex::D,
        4 => engine::FileIndex::E,
        5 => engine::FileIndex::F,
        6 => engine::FileIndex::G,
        7 => engine::FileIndex::H,
        _ => unreachable!(),
    }
}

pub fn rank_index_clamp(index: i8) -> engine::RankIndex {
    match index.clamp(0, 7) {
        0 => engine::RankIndex::_1,
        1 => engine::RankIndex::_2,
        2 => engine::RankIndex::_3,
        3 => engine::RankIndex::_4,
        4 => engine::RankIndex::_5,
        5 => engine::RankIndex::_6,
        6 => engine::RankIndex::_7,
        7 => engine::RankIndex::_8,
        _ => unreachable!(),
    }
}
