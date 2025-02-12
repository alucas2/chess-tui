use engine::{GameState, MoveFlag, PieceKind, PlayerSide, ScoreInfo, Search, SquareIndex};
use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode},
    layout::{Constraint, Layout, Margin},
    style::{Color, Style, Styled, Stylize},
    widgets::{Block, Gauge, Padding, Paragraph, Widget},
};

use crate::{custom_widgets, game_state::GameStateHistory, moves::MoveWithNotation, style, util};

use super::{
    newgame_popup::NewGamePopup, promotion_popup::PromotionPopup, IState, State, UiLayout,
};

/// State: interact with the board, make moves, etc.
pub struct Home {
    board_rotated: bool,
    gamestate: GameStateHistory,
    cursor: SquareIndex,
    pending_move_start: Option<SquareIndex>,
    pending_search: Option<Search>,
}

impl Home {
    pub fn new(gs: GameState) -> Home {
        Home {
            board_rotated: false,
            gamestate: GameStateHistory::new(gs),
            cursor: SquareIndex::A1,
            pending_move_start: None,
            pending_search: None,
        }
    }

    pub fn current(&self) -> &GameState {
        self.gamestate.current()
    }

    pub fn available_moves(&self) -> &[MoveWithNotation] {
        self.gamestate.available_moves()
    }

    pub fn do_move(&mut self, mv: MoveWithNotation) {
        self.gamestate.do_move(mv);
        self.pending_move_start = None;
        self.pending_search = None;
    }

    pub fn undo(&mut self) {
        if self.gamestate.undo() {
            self.pending_move_start = None;
            self.pending_search = None;
        }
    }

    pub fn redo(&mut self) {
        if self.gamestate.redo() {
            self.pending_move_start = None;
            self.pending_search = None;
        }
    }

    fn update_cursor(&mut self, keycode: KeyCode) {
        let (mut dx, mut dy) = match keycode {
            KeyCode::Left => (-1, 0),
            KeyCode::Right => (1, 0),
            KeyCode::Up => (0, 1),
            KeyCode::Down => (0, -1),
            _ => return,
        };
        if self.board_rotated {
            dx = -dx;
            dy = -dy;
        }
        let (file, rank) = self.cursor.coords();
        let file = util::file_index_clamp(file as i8 + dx);
        let rank = util::rank_index_clamp(rank as i8 + dy);
        self.cursor = SquareIndex::from_coords(file, rank)
    }

    fn draw_previous_move(
        &self,
        layout: &UiLayout,
        board_layout: custom_widgets::Board,
        buf: &mut Buffer,
    ) {
        let Some(mv) = self.gamestate.moves_before().last() else {
            return;
        };
        let highlight = custom_widgets::ColoredSquare {
            bg: style::color::HIGHLIGHT_PREVIOUS_MOVE,
            bg_blend_factor: 0.5,
            ..Default::default()
        };
        highlight.render(board_layout.inner(mv.start, layout.board), buf);
        highlight.render(board_layout.inner(mv.end, layout.board), buf);
    }

    fn draw_possible_moves(
        &self,
        layout: &UiLayout,
        board_layout: custom_widgets::Board,
        buf: &mut Buffer,
    ) {
        let Some(start) = self.pending_move_start else {
            return;
        };
        let highlight = custom_widgets::ColoredSquare {
            bg: style::color::HIGHLIGHT_POSSIBLE_MOVE,
            bg_blend_factor: 0.7,
            ..Default::default()
        };
        highlight.render(board_layout.inner(start, layout.board), buf);
        let available_moves = self.gamestate.available_moves().iter();
        for mv in available_moves.filter(|mv| mv.start == start) {
            match mv.flag {
                // Avoid highlighting the same square multiple times for promotions
                MoveFlag::Promotion(x) if x != PieceKind::Queen => {}
                _ => highlight.render(
                    board_layout.inner(mv.end, layout.board).inner(Margin {
                        horizontal: 2,
                        vertical: 1,
                    }),
                    buf,
                ),
            }
        }
    }

    fn draw_current_selection(
        &self,
        layout: &UiLayout,
        board_layout: custom_widgets::Board,
        buf: &mut Buffer,
    ) {
        custom_widgets::ColoredSquare {
            bottom_left_label: Some(self.cursor.coords().0.label()),
            top_right_label: Some(self.cursor.coords().1.label()),
            top_left_label: Some('┏'),
            bottom_right_label: Some('┛'),
            fg: style::color::HIGHLIGHT_CURSOR,
            ..Default::default()
        }
        .render(board_layout.inner(self.cursor, layout.board), buf);
    }

    fn draw_pieces(
        &self,
        layout: &UiLayout,
        board_layout: custom_widgets::Board,
        buf: &mut Buffer,
    ) {
        for square in SquareIndex::iter() {
            if let Some((color, kind)) = self.gamestate.current().piece(square) {
                custom_widgets::Piece { kind, side: color }
                    .render(board_layout.inner(square, layout.board), buf);
            }
        }
    }

    fn draw_position_panel(&self, layout: &UiLayout, buf: &mut Buffer) {
        let fen = self.gamestate.current().to_string();
        Paragraph::new(fen)
            .block(Block::bordered().title("Position"))
            .render(layout.fen_panel, buf);
    }

    fn draw_moves_history_panel(&self, layout: &UiLayout, buf: &mut Buffer) {
        let block = Block::bordered().title("Moves");
        let mut text =
            custom_widgets::WordWrappedParagraph::new(block.inner(layout.moves_panel).width);
        let mut style = Style::default();
        let moves_before = self.gamestate.moves_before().iter();
        let moves_after = self.gamestate.moves_after().iter();
        for (i, mv) in moves_before.chain(moves_after).enumerate() {
            if i == 0 && mv.player == PlayerSide::Black {
                text.push_word(format!("{}.", mv.number).set_style(style).reversed());
                text.push_word("..");
            }
            if i == self.gamestate.moves_before().len() {
                style = style.fg(Color::Red) // Change the style for moves after
            }
            if mv.player == PlayerSide::White {
                text.push_word(format!("{}.", mv.number).set_style(style).reversed());
            }
            text.push_word(format!("{}", mv).set_style(style));
        }
        text.render(block.inner(layout.moves_panel), buf);
        block.render(layout.moves_panel, buf);
    }

    fn draw_ponder_panel(&self, layout: &UiLayout, buf: &mut Buffer) {
        let Some(result) = self.pending_search.as_ref().map(|s| s.status()) else {
            return;
        };
        let mut block = Block::bordered().title(format!("Analysis"));
        let [gauge_area, paragraph_area] =
            Layout::vertical([Constraint::Length(1), Constraint::Fill(1)])
                .areas(block.inner(layout.ponder_panel));

        // Add a cute spinner if the computer is still pondering
        if result.thinking {
            let spinner = {
                use std::sync::OnceLock;
                use std::time::Instant;
                static START: OnceLock<Instant> = OnceLock::new();
                let secs = START.get_or_init(Instant::now).elapsed().as_secs_f32();
                style::SPINNER_SYMBOL[(secs * 10.0) as usize % style::SPINNER_SYMBOL.len()]
            };
            block = block.title(spinner.to_string());
        }

        // Show a gauge with a white and a black part to represent the score
        let (score_label, mut gauge_ratio) = match result.score {
            ScoreInfo::Normal(x) => {
                // Map the i16 score to a percentage using a totally arbitrary formula
                let r = x as f64 / i16::MAX as f64;
                let r = (r * 60.0).atan() * std::f64::consts::FRAC_1_PI + 0.5;
                (x.to_string(), r)
            }
            ScoreInfo::Win(x) => (format!("M{x}"), 1.0),
            ScoreInfo::Loose(x) => (format!("-M{x}"), 0.0),
        };
        if self.gamestate.current().side_to_move() == PlayerSide::Black {
            gauge_ratio = 1.0 - gauge_ratio;
        }
        Gauge::default()
            .label(&score_label)
            .ratio(gauge_ratio)
            .use_unicode(true)
            .gauge_style(
                Style::default()
                    .fg(style::color::WHITE_PIECE)
                    .bg(style::color::BLACK_PIECE),
            )
            .render(gauge_area, buf);

        let depth = result.depth;
        let stats = result.stats;
        let best = match result.best {
            Some(best) => {
                let mut available_moves = self.gamestate.available_moves().iter();
                &available_moves
                    .find(|mv| mv.inner == best)
                    .expect("Search should have found a legal move")
                    .to_string()
            }
            None => "None",
        };
        let text = format!("Depth: {depth}\nBest: {best}\n{stats:#?}");
        block.render(layout.ponder_panel, buf);
        Paragraph::new(text).render(paragraph_area, buf);
    }

    fn draw_message_panel(&self, layout: &UiLayout, buf: &mut Buffer) {
        let white_block = Block::new()
            .padding(Padding::uniform(1))
            .bg(style::color::WHITE_PIECE)
            .fg(style::color::BLACK_PIECE);
        let black_block = Block::new()
            .padding(Padding::uniform(1))
            .bg(style::color::BLACK_PIECE)
            .fg(style::color::WHITE_PIECE);
        let (text, block) = match (
            self.gamestate.current().side_to_move(),
            self.gamestate.current().is_check(),
            self.gamestate.available_moves().is_empty(),
        ) {
            (PlayerSide::White, true, true) => ("Black won the game!", black_block),
            (PlayerSide::Black, true, true) => ("White won the game!", white_block),
            (PlayerSide::White, false, true) => ("Stalemate!", white_block),
            (PlayerSide::Black, false, true) => ("Stalemate!", black_block),
            (PlayerSide::White, _, false) => ("White to move", white_block),
            (PlayerSide::Black, _, false) => ("Black to move", black_block),
        };
        Paragraph::new(text)
            .centered()
            .block(block)
            .render(layout.message_panel, buf);
    }
}

impl IState for Home {
    fn update(mut self: Box<Self>, event: Event) -> State {
        match util::to_keycode(event) {
            Some(KeyCode::Esc) => return State::Halted,
            Some(KeyCode::Char('r')) => self.board_rotated = !self.board_rotated,
            Some(KeyCode::Char('n')) => return State::Running(Box::new(NewGamePopup::new(self))),
            Some(KeyCode::Char('z')) => self.undo(),
            Some(KeyCode::Char('y')) => self.redo(),
            Some(KeyCode::Char('s')) => self.pending_search = None,
            Some(KeyCode::Char('a')) => match &self.pending_search {
                Some(search) => {
                    if let Some(best) = search.status().best {
                        let mut available_moves = self.gamestate.available_moves().iter();
                        let mv = available_moves
                            .find(|mv| mv.inner == best)
                            .expect("Search should have found a legal move");
                        self.do_move(*mv);
                    }
                }
                None => self.pending_search = Some(Search::start(*self.gamestate.current())),
            },
            Some(KeyCode::Backspace) => self.pending_move_start = None,
            Some(KeyCode::Enter) => {
                let mut available_moves = self.gamestate.available_moves().iter();
                match self.pending_move_start {
                    Some(start) => {
                        if let Some(&mv) =
                            available_moves.find(|mv| mv.start == start && mv.end == self.cursor)
                        {
                            if let MoveFlag::Promotion(_) = mv.flag {
                                return State::Running(Box::new(PromotionPopup::new(self, mv.end)));
                            } else {
                                self.do_move(mv);
                            }
                        }
                        self.pending_move_start = None;
                    }
                    None => {
                        if available_moves.any(|mv| mv.start == self.cursor) {
                            self.pending_move_start = Some(self.cursor)
                        }
                    }
                }
            }
            Some(k @ (KeyCode::Left | KeyCode::Right | KeyCode::Up | KeyCode::Down)) => {
                self.update_cursor(k)
            }
            _ => {}
        };
        State::Running(self)
    }

    fn bottom_bar(&self) -> custom_widgets::BottomBar {
        let mut bottom_bar = custom_widgets::BottomBar::default();
        bottom_bar.push("Esc", "Quit");
        bottom_bar.push("N", "New game");
        bottom_bar.push("R", "Rotate board");
        bottom_bar.push("←/↑/→/↓", "Select");
        bottom_bar.push("Z/Y", "Undo/Redo move");
        match self.pending_move_start {
            Some(_) => bottom_bar.push("Backspace", "Cancel move"),
            None => bottom_bar.push("Enter", "Move piece"),
        };
        match &self.pending_search {
            Some(search) => {
                bottom_bar.push("S", "Stop analysing");
                if search.status().best.is_some() {
                    bottom_bar.push("A", "Auto move")
                }
            }
            None => bottom_bar.push("A", "Analyse"),
        }
        bottom_bar
    }

    fn draw(&mut self, layout: &UiLayout, buf: &mut Buffer) {
        let board = custom_widgets::Board {
            rotated: self.board_rotated,
        };
        board.render(layout.board, buf);
        self.draw_previous_move(layout, board, buf);
        self.draw_possible_moves(layout, board, buf);
        self.draw_current_selection(layout, board, buf);
        self.draw_pieces(layout, board, buf);
        self.draw_position_panel(layout, buf);
        self.draw_moves_history_panel(layout, buf);
        self.draw_ponder_panel(layout, buf);
        self.draw_message_panel(layout, buf);
    }
}
