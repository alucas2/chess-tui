mod edit_fen;
mod home;
mod newgame_popup;
mod promotion_popup;

use home::Home;
use ratatui::{
    buffer::Buffer,
    crossterm::event::Event,
    layout::{Constraint, Layout, Rect},
    widgets::Widget,
    Frame,
};

use crate::{custom_widgets, fen};

pub struct Ui {
    state: State,
}

struct UiLayout {
    board: Rect,
    bottom_bar: Rect,
    message_panel: Rect,
    fen_panel: Rect,
    moves_panel: Rect,
    ponder_panel: Rect,
}

enum State {
    Halted,
    Running(Box<dyn IState>),
}

trait IState {
    fn update(self: Box<Self>, event: Event) -> State;
    fn bottom_bar(&self) -> custom_widgets::BottomBar;
    fn draw(&mut self, layout: &UiLayout, buf: &mut Buffer);
}

impl Ui {
    pub fn new() -> Self {
        Ui {
            state: State::Running(Box::new(Home::new(fen::initial_position()))),
        }
    }

    pub fn handle_event(&mut self, event: Event) {
        let current_state = std::mem::replace(&mut self.state, State::Halted);
        self.state = match current_state {
            State::Halted => State::Halted,
            State::Running(s) => s.update(event),
        }
    }

    pub fn halted(&self) -> bool {
        matches!(self.state, State::Halted)
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        let layout = UiLayout::new(frame.area());
        match &mut self.state {
            State::Halted => {}
            State::Running(s) => {
                let buf = frame.buffer_mut();
                s.draw(&layout, buf);
                s.bottom_bar().render(layout.bottom_bar, buf);
            }
        }
    }
}

impl UiLayout {
    fn new(area: Rect) -> UiLayout {
        let [top, _, bottom_bar] = Layout::vertical([
            Constraint::Length(custom_widgets::Board::HEIGHT),
            Constraint::Fill(1),
            Constraint::Length(1),
        ])
        .areas(area);
        let [board, side_panel, _] = Layout::horizontal([
            Constraint::Length(custom_widgets::Board::WIDTH),
            Constraint::Fill(1),
            Constraint::Length(1),
        ])
        .spacing(1)
        .areas(top);
        let [message_panel, _, fen_panel, moves_panel, ponder_panel] = Layout::vertical([
            Constraint::Length(3),
            Constraint::Length(1),
            Constraint::Length(3),
            Constraint::Length(20),
            Constraint::Fill(1),
        ])
        .areas(side_panel);
        UiLayout {
            board,
            bottom_bar,
            message_panel,
            fen_panel,
            moves_panel,
            ponder_panel,
        }
    }
}
