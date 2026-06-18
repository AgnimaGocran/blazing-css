# Changelog

## [0.1.14] - 2026-05-31

Media queries don't support CSS custom properties, so the previously
generated var(--bp-*) breakpoints were ignored by browsers.

### Fixed
- Refactor media query formatting to use iterator chains
- Remove dead get_bp_name helper
- Add known limitations section to README

## [0.1.13] - 2026-05-22

### Added
- Support for CSS keyframes
- Nested pseudo-classes and pseudo-selectors
- CSS blocks and attributes normalization from macro
- Support for different folders/files as outputs
- Breakpoints support
- Version output on startup

### Fixed
- Important fixes for hash calculations
- Fixed hash computation bugs
- Changed `$` sign to `@` for variable syntax
- Support for `$crate` syntax to explicitly specify target crate
- Project creation from workspace when using `$crate` syntax
- Handling of paths starting with `/` (from empty shell variables)
- Path handling bugs
- Fatal error fix during style rendering

### Changed
- Binary name changed to `bzc`
- Various internal improvements and refactoring

## [0.1.12] - Previous versions

Initial release of blazing-css - a CSS preprocessing tool for Rust projects.
