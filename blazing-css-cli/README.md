# blazing-css-cli

`bzc` watches Rust sources (by default `src/**/*.rs`) and regenerates CSS files produced by the `blazing-css` macro crate.  

## Usage

```bash
bzc render [--project=ui,web] output/styles.css
bzc watch [--project=ui,web] output/styles.css
```

- Without `-p/--project`, the current directory is treated as a single project.
- With `-p/--project`, each listed project is resolved from the current workspace `Cargo.toml` first, then by matching directories in the current directory.
- The output path is resolved relative to each project root (unless you provide an absolute path).

`render` runs a single pass. `watch` performs an initial render and continues rebuilding CSS every time a watched Rust source file changes.
