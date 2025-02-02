use engine::{MoveFlag, PieceKind, SquareIndex};
use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode},
    style::{Color, Stylize},
    widgets::{Block, Clear, List, ListState, StatefulWidget, Widget},
};

use crate::{custom_widgets, moves::MoveWithNotation, util};

use super::{home::Home, IState, State, UiLayout};

/// State: popup menu asking for which piece to promote to
pub struct PromotionPopup {
    home: Box<Home>,
    options: Vec<MoveWithNotation>,
    list: ListState,
}

impl PromotionPopup {
    pub fn new(parent: Box<Home>, end: SquareIndex) -> PromotionPopup {
        let available_moves = parent.available_moves().iter().copied();
        let options = available_moves
            .filter(|mv| mv.end == end && matches!(mv.flag, MoveFlag::Promotion(_)))
            .collect();
        PromotionPopup {
            home: parent,
            options,
            list: ListState::default().with_selected(Some(0)),
        }
    }
}

impl IState for PromotionPopup {
    fn update(mut self: Box<Self>, event: Event) -> State {
        match util::to_keycode(event) {
            Some(KeyCode::Esc) => return State::Running(self.home),
            Some(KeyCode::Up) => self.list.select_previous(),
            Some(KeyCode::Down) => self.list.select_next(),
            Some(KeyCode::Enter) => match self.list.selected() {
                Some(i) => {
                    self.home.do_move(self.options[i]);
                    return State::Running(self.home);
                }
                _ => {}
            },
            _ => {}
        }
        State::Running(self)
    }

    fn bottom_bar(&self) -> custom_widgets::BottomBar {
        let mut bottom_bar = custom_widgets::BottomBar::default();
        bottom_bar.push("Esc", "Cancel");
        bottom_bar.push("↑/↓", "Select");
        bottom_bar.push("Enter", "Confirm");
        bottom_bar
    }

    fn draw(&mut self, layout: &UiLayout, buf: &mut Buffer) {
        self.home.draw(layout, buf);
        let popup_area = util::layout_centered(22, 2 + self.options.len() as u16, layout.board);
        Clear.render(popup_area, buf);
        StatefulWidget::render(
            List::new(self.options.iter().map(|mv| match mv.flag {
                MoveFlag::Promotion(PieceKind::Knight) => "Knight",
                MoveFlag::Promotion(PieceKind::Bishop) => "Bishop",
                MoveFlag::Promotion(PieceKind::Rook) => "Rook",
                MoveFlag::Promotion(PieceKind::Queen) => "Queen",
                _ => unreachable!(),
            }))
            .highlight_symbol("> ")
            .block(Block::bordered().title("Promote").fg(Color::Yellow)),
            popup_area,
            buf,
            &mut self.list,
        );
    }
}
