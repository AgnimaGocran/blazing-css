use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    blazing_css::render_css("styles.css")
}
