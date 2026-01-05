# blazing-css

`blazing-css` is a small Rust toolchain for generating CSS classes during your build. You write styles directly in the `css! { ... }` macro, get  a stable hashed class name back, and let either the build-script helper or the CLI (`bzc`) gather every macro call into a final `styles.css`.

## Repository layout
- `blazing-css` — public API; re-exports the `css!` macro and exposes `render_css(_with_options)` helpers for build scripts.
- `blazing-css-macro` — the procedural macro that canonicalizes CSS segments and produces the hashed class.
- `blazing-css-core` — shared parsing, normalization, and hashing utilities.
- `blazing-css-cli` (`bzc`) — command-line tool that renders once or watches your sources and rewrites CSS on every change.
- `example-project` — a minimal consumer; use it as a reference template.

## Installation

### Library
Pull the crate straight from Git and declare it in both dependency sections:

```bash
cargo add blazing-css --git https://github.com/AgnimaGocran/blazing-css
cargo add blazing-css --git https://github.com/AgnimaGocran/blazing-css --build
```

Set up a `build.rs` that writes CSS relative to `CARGO_MANIFEST_DIR`:

```rust
// build.rs
fn main() -> Result<(), Box<dyn std::error::Error>> {
    blazing_css::render_css("assets/styles.css")
}
```

Inside your Rust sources, call the macro; it returns the hashed class string:

```rust
use blazing_css::css;

fn render_button() -> String {
    format!(
        r#"<button class="{class}">Button</button>"#,
        class = css! {
            padding: 12px 20px;
            background: linear-gradient(90deg, #ff7a18, #af002d 70%);
            border: none;
            color: white;
        }
    )
}
```

After the build, `assets/styles.css` will contain something like:

```css
.AbCdE {
	color: white;
	padding: 12px 20px;
	/* ... */
}
```

Make sure the generated CSS gets bundled or copied next to your binary as part of your deployment pipeline.

### CLI
Install the CLI if you prefer rendering/watching outside of `build.rs`:

```bash
cargo install blazing-css-cli --git https://github.com/AgnimaGocran/blazing-css
```

Usage:

```bash
# Single render
bzc render [--project web,ui] assets/styles.css

# Watch for changes
bzc watch  [--project web,ui] assets/styles.css
```

- Without `--project`, the current directory is treated as a single project.
- With `--project`, `bzc` resolves each entry via the workspace `Cargo.toml`, then falls back to matching directories in the current folder.
- `bzc` scans `src/**/*.rs`, tracks changes, and rewrites the CSS file every time.

## License
[MIT](LICENSE)
