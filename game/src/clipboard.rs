use std::sync::{Mutex, MutexGuard, OnceLock};

fn get_clipboard() -> MutexGuard<'static, Option<arboard::Clipboard>> {
    static CLIPBOARD: OnceLock<Mutex<Option<arboard::Clipboard>>> = OnceLock::new();
    CLIPBOARD
        .get_or_init(|| Mutex::new(arboard::Clipboard::new().ok()))
        .lock()
        .unwrap()
}

pub fn get() -> String {
    match get_clipboard().as_mut() {
        Some(c) => c.get_text().unwrap_or_default(),
        None => String::default(),
    }
}

pub fn set(text: impl ToString) {
    match get_clipboard().as_mut() {
        Some(c) => {
            let _ = c.set_text(text.to_string());
        }
        None => {}
    }
}
