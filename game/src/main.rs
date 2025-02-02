use std::time::Duration;

use ratatui::crossterm::event::{self, Event};

fn main() {
    let _ = run();
    ratatui::restore();
}

fn run() -> std::io::Result<()> {
    let mut terminal = ratatui::init();
    let mut game = game::ui::Ui::new();
    while !game.halted() {
        terminal.draw(|f| game.draw(f))?;
        let has_event = event::poll(Duration::from_millis(100))?;
        if has_event {
            let event = event::read()?;
            match event {
                Event::Key(..) => game.handle_event(event),
                _ => continue,
            }
        }
    }
    Ok(())
}
