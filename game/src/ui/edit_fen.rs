use engine::{GameState, SquareIndex};
use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode},
    style::{Color, Stylize},
    widgets::{Block, Clear, Widget},
};

use crate::{custom_widgets, fen, util};

use super::{home::Home, IState, State, UiLayout};

/// State: editing a custom position before starting a new game
pub struct EditFen {
    parent: Box<dyn IState>,
    textarea: custom_widgets::TextArea,
    parsed_gamestate: GameState,
    valid: bool,
}

impl EditFen {
    pub fn new(parent: Box<dyn IState>, gamestate: GameState) -> EditFen {
        EditFen {
            parent,
            textarea: custom_widgets::TextArea::new(fen::unparse(&gamestate)),
            parsed_gamestate: gamestate,
            valid: true,
        }
    }

    fn draw_pieces(
        &self,
        layout: &UiLayout,
        board_layout: custom_widgets::Board,
        buf: &mut Buffer,
    ) {
        for square in SquareIndex::iter() {
            if let Some((color, kind)) = self.parsed_gamestate.piece(square) {
                custom_widgets::Piece { kind, color }
                    .render(board_layout.inner(square, layout.board), buf);
            }
        }
    }
}

impl IState for EditFen {
    fn update(mut self: Box<Self>, event: Event) -> State {
        match util::to_keycode(event.clone()) {
            Some(KeyCode::Esc) => return State::Running(self.parent),
            Some(KeyCode::Enter) => {
                if self.valid {
                    return State::Running(Box::new(Home::new(self.parsed_gamestate)));
                }
            }
            _ => {
                if self.textarea.update(event) {
                    match fen::parse(self.textarea.text()) {
                        Ok(gs) => {
                            self.valid = true;
                            self.parsed_gamestate = gs;
                        }
                        Err(_) => self.valid = false,
                    }
                }
            }
        }
        State::Running(self)
    }

    fn bottom_bar(&self) -> custom_widgets::BottomBar {
        let mut bottom_bar = custom_widgets::BottomBar::default();
        bottom_bar.push("Esc", "Cancel");
        if self.valid {
            bottom_bar.push("Enter", "Confirm");
        }
        bottom_bar
    }

    fn draw(&mut self, layout: &UiLayout, buf: &mut Buffer) {
        let board = custom_widgets::Board { rotated: false };
        board.render(layout.board, buf);
        self.draw_pieces(layout, board, buf);
        let block = if self.valid {
            Block::bordered().title("Position").fg(Color::Yellow)
        } else {
            Block::bordered().title("Invalid Position").fg(Color::Red)
        };
        self.textarea.inner.set_block(block);
        Clear.render(layout.fen_panel, buf);
        self.textarea.render(layout.fen_panel, buf);
    }
}
