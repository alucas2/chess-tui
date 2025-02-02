use engine::GameState;

use crate::moves::{self, MoveWithNotation};

pub struct GameStateHistory {
    initial: GameState,
    current: GameState,
    available_moves: Vec<MoveWithNotation>,
    move_history: Vec<MoveWithNotation>,
    position_in_history: usize,
}

impl GameStateHistory {
    pub fn new(initial: GameState) -> Self {
        GameStateHistory {
            available_moves: moves::moves_with_notation(&initial),
            initial,
            current: initial,
            move_history: vec![],
            position_in_history: 0,
        }
    }

    pub fn current(&self) -> &GameState {
        &self.current
    }

    pub fn available_moves(&self) -> &[MoveWithNotation] {
        &self.available_moves
    }

    pub fn moves_before(&self) -> &[MoveWithNotation] {
        &self.move_history[..self.position_in_history]
    }

    pub fn moves_after(&self) -> &[MoveWithNotation] {
        &self.move_history[self.position_in_history..]
    }

    pub fn do_move(&mut self, mv: MoveWithNotation) {
        assert!(self.available_moves.contains(&mv));
        self.move_history.drain(self.position_in_history..);
        self.move_history.push(mv);
        self.current = self.current.do_move(mv.inner).unwrap();
        self.available_moves = moves::moves_with_notation(&self.current);
        self.position_in_history += 1;
    }

    pub fn undo(&mut self) -> bool {
        if self.position_in_history == 0 {
            return false;
        }
        self.current = self.initial;
        for mv in &self.move_history[..(self.position_in_history - 1)] {
            self.current = self.current.do_move(mv.inner).unwrap()
        }
        self.available_moves = moves::moves_with_notation(&self.current);
        self.position_in_history -= 1;
        true
    }

    pub fn redo(&mut self) -> bool {
        if self.position_in_history == self.move_history.len() {
            return false;
        }
        let mv = self.move_history[self.position_in_history];
        self.current = self.current.do_move(mv.inner).unwrap();
        self.available_moves = moves::moves_with_notation(&self.current);
        self.position_in_history += 1;
        true
    }
}
