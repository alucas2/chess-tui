use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode},
    style::{Color, Stylize},
    widgets::{Block, Clear, List, ListState, StatefulWidget, Widget},
};

use crate::{custom_widgets, fen, util};

use super::{edit_fen::EditFen, home::Home, IState, State, UiLayout};

/// State: popup menu asking how to start a new game
pub struct NewGamePopup {
    home: Box<Home>,
    list: ListState,
}

impl NewGamePopup {
    pub fn new(parent: Box<Home>) -> NewGamePopup {
        NewGamePopup {
            home: parent,
            list: ListState::default().with_selected(Some(0)),
        }
    }
}

impl IState for NewGamePopup {
    fn update(mut self: Box<Self>, event: Event) -> State {
        match util::to_keycode(event) {
            Some(KeyCode::Esc) => return State::Running(self.home),
            Some(KeyCode::Up) => self.list.select_previous(),
            Some(KeyCode::Down) => self.list.select_next(),
            Some(KeyCode::Enter) => match self.list.selected() {
                Some(0) => {
                    let gs = fen::initial_position();
                    return State::Running(Box::new(Home::new(gs)));
                }
                Some(1) => {
                    let gs = *self.home.current();
                    return State::Running(Box::new(EditFen::new(self, gs)));
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
        let popup_area = util::layout_centered(22, 4, layout.board);
        Clear.render(popup_area, buf);
        StatefulWidget::render(
            List::new(["Initial position", "Custom position"])
                .highlight_symbol("> ")
                .block(Block::bordered().title("New Game").fg(Color::Yellow)),
            popup_area,
            buf,
            &mut self.list,
        );
    }
}
