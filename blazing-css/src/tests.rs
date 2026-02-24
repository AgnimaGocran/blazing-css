use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use blazing_css_core::CssBlock;

use super::{css, format_css_block, process_file};

#[test]
fn format_nested_block_emits_pseudo_class() {
	let block = CssBlock {
		selector_suffix: String::new(),
		segments: vec!["color: red".into(), "padding: 10px".into()],
		children: vec![CssBlock {
			selector_suffix: ":hover".into(),
			segments: vec!["color: blue".into(), "background: yellow".into()],
			children: vec![],
		}],
	};
	let css = format_css_block("AbCdEf", &block, None);
	assert!(css.contains(".AbCdEf {"));
	assert!(css.contains("color: red"));
	assert!(css.contains(".AbCdEf:hover {"));
	assert!(css.contains("color: blue"));
	assert!(css.contains("background: yellow"));
}

#[test]
fn format_nested_block_emits_pseudo_element() {
	let block = CssBlock {
		selector_suffix: String::new(),
		segments: vec![],
		children: vec![CssBlock {
			selector_suffix: "::before".into(),
			segments: vec!["content: \"\"".into(), "display: block".into()],
			children: vec![],
		}],
	};
	let css = format_css_block("XyZ", &block, None);
	assert!(css.contains(".XyZ::before {"));
	assert!(css.contains("content: \"\""));
	assert!(css.contains("display: block"));
}

#[test]
fn format_deeply_nested_emits_concatenated_selector() {
	let block = CssBlock {
		selector_suffix: String::new(),
		segments: vec!["outline: none".into()],
		children: vec![CssBlock {
			selector_suffix: ":focus".into(),
			segments: vec!["outline: none".into()],
			children: vec![CssBlock {
				selector_suffix: "::after".into(),
				segments: vec!["content: \"focused\"".into()],
				children: vec![],
			}],
		}],
	};
	let css = format_css_block("Root", &block, None);
	assert!(css.contains(".Root:focus {"));
	assert!(css.contains(".Root:focus::after {"));
	assert!(css.contains("content: \"focused\""));
}

#[test]
fn format_block_scopes_keyframes_and_animation_names() {
	let block = CssBlock {
		selector_suffix: String::new(),
		segments: vec![
			"animation: engine-pulse 0.8s ease-in-out infinite alternate".into(),
			"background: rgb(234, 179, 8)".into(),
		],
		children: vec![CssBlock {
			selector_suffix: "@keyframes engine-pulse".into(),
			segments: vec![],
			children: vec![
				CssBlock {
					selector_suffix: "from".into(),
					segments: vec!["opacity: 0.3".into()],
					children: vec![],
				},
				CssBlock {
					selector_suffix: "to".into(),
					segments: vec!["opacity: 1.0".into()],
					children: vec![],
				},
			],
		}],
	};

	let css = format_css_block("HashId", &block, None);
	assert!(css.contains(".HashId {"));
	assert!(css.contains(
		"animation: engine-pulse-HashId 0.8s ease-in-out infinite alternate;"
	));
	assert!(css.contains("@keyframes engine-pulse-HashId {"));
	assert!(css.contains("\tfrom {"));
	assert!(css.contains("\tto {"));
	assert!(!css.contains(".HashId@keyframes"));
}

#[test]
fn process_file_parses_nested_hover_block_from_rust_source() {
	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let path = std::env::temp_dir().join(format!("blazing_css_nested_hover_{unique}.rs"));
	let source = r#"
		fn demo() {
			let _ = css! {
				display: inline-flex;
				align-items: center;
				gap: 0.15rem;
				padding: 0.1rem 0.35rem;
				border-radius: 4px;
				cursor: pointer;
				font-size: 0.85rem;
				&:hover { background: rgb(241, 245, 249); }
			};
		}
	"#;
	fs::write(&path, source).unwrap();

	let entries = process_file(&path).unwrap();
	fs::remove_file(&path).unwrap();

	assert_eq!(entries.len(), 1);
	let block = &entries[0].block;
	assert_eq!(block.children.len(), 1);
	assert_eq!(block.children[0].selector_suffix, ":hover");
	assert_eq!(block.children[0].segments, vec!["background: rgb(241, 245, 249)"]);

	let css = format_css_block("KZHQOo", block, None);
	assert!(css.contains(".KZHQOo:hover {"));
	assert!(!css.contains("&: hover"));
}

/// Обрабатывает файл repertoire.rs.test один в один (include_str!).
/// Запуск: cargo test -p blazing-css repertoire_css_output -- --nocapture
#[test]
fn repertoire_css_output() {
	let source = include_str!("repertoire.rs.test");

	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let path = std::env::temp_dir().join(format!("blazing_css_repertoire_{unique}.rs"));
	fs::write(&path, source).unwrap();

	let entries = process_file(&path).unwrap();
	fs::remove_file(&path).unwrap();

	let mut full = String::new();
	for (i, entry) in entries.iter().enumerate() {
		let css = format_css_block(&entry.hash, &entry.block, None);
		full.push_str(&format!("/* block {} hash {} */\n{}\n\n", i + 1, entry.hash, css));
	}

	eprintln!("Generated CSS ({} blocks):\n{}", entries.len(), full);

	assert!(!entries.is_empty(), "должен быть хотя бы один css! блок");
	assert!(
		full.contains("@keyframes"),
		"ожидался @keyframes dot-pulse в выводе"
	);
	assert!(
		!full.contains("&:hover") && !full.contains("&: hover"),
		"&:hover должен быть развёрнут в селектор, не оставаться как &"
	);
}

/// Проверяет, что макрос css! и process_file дают одинаковый хеш
/// для `display: grid; justify-content: center;`
#[test]
fn macro_and_process_file_produce_same_hash() {
	// Хеш из макроса (compile-time)
	let macro_hash = css! {
		display: grid;
		justify-content: center;
	};

	// Хеш из process_file (runtime, как делает CLI)
	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let path = std::env::temp_dir().join(format!("blazing_css_hash_check_{unique}.rs"));
	let source = r#"
		fn demo() {
			let _ = css! {
				display: grid;
				justify-content: center;
			};
		}
	"#;
	fs::write(&path, source).unwrap();
	let entries = process_file(&path).unwrap();
	fs::remove_file(&path).unwrap();

	assert_eq!(entries.len(), 1);
	let cli_hash = &entries[0].hash;

	eprintln!("macro hash:        {}", macro_hash);
	eprintln!("process_file hash: {}", cli_hash);

	assert_eq!(
		macro_hash, cli_hash.as_str(),
		"macro css! and process_file must produce identical hashes"
	);
}

/// Воспроизведение бага: число+единица без пробела (0.02em) парсится rustc как float
/// с экспонентой → "expected at least one digit in exponent".
/// Тест проверяет, что компиляция крейта с css! { letter-spacing: 0.02em; } действительно падает.
#[test]
fn letter_spacing_0_02em_without_space_fails_compile() {
	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
	let blaze_path = manifest_dir
		.canonicalize()
		.unwrap()
		.display()
		.to_string()
		.replace('\\', "/");
	let tmp = std::env::temp_dir().join(format!("blazing_css_0_02em_bug_{unique}"));
	let _ = fs::create_dir_all(tmp.join("src"));
	let cargo_toml = format!(
		r#"[package]
name = "compile_fail_0_02em"
version = "0.0.0"
edition = "2021"

[dependencies]
blazing-css = {{ path = "{}" }}
"#,
		blaze_path
	);
	let main_rs = r#"
use blazing_css::css;

fn main() {
	let _ = css! {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		font-size: 1.1rem;
		font-weight: 700;
		letter-spacing: 0.02em;
	};
}
"#;
	fs::write(tmp.join("Cargo.toml"), cargo_toml).unwrap();
	fs::write(tmp.join("src/main.rs"), main_rs).unwrap();
	let status = std::process::Command::new("cargo")
		.args(["build", "--quiet"])
		.current_dir(&tmp)
		.output()
		.unwrap();
	let _ = fs::remove_dir_all(&tmp);
	assert!(
		!status.status.success(),
		"ожидалась ошибка компиляции (0.02em без пробела); stderr: {}",
		String::from_utf8_lossy(&status.stderr)
	);
	assert!(
		String::from_utf8_lossy(&status.stderr).contains("exponent"),
		"ожидалось сообщение про exponent в stderr"
	);
}

/// После фикса: вариант с пробелом (0.02 em) компилируется и даёт каноничный letter-spacing: 0.02em.
#[test]
fn letter_spacing_0_02_em_with_space_works() {
	let hash = css! {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		font-size: 1.1rem;
		font-weight: 700;
		letter-spacing: 0.02 em;
	};
	// Проверяем, что process_file даёт тот же хеш при записи "0.02 em"
	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let path = std::env::temp_dir().join(format!("blazing_css_0_02_em_ok_{unique}.rs"));
	let source = r#"
		fn navbar() {
			let _ = css! {
				display: flex;
				align-items: center;
				gap: 0.5rem;
				font-size: 1.1rem;
				font-weight: 700;
				letter-spacing: 0.02 em;
			};
		}
	"#;
	fs::write(&path, source).unwrap();
	let entries = process_file(&path).unwrap();
	fs::remove_file(&path).unwrap();
	assert_eq!(entries.len(), 1);
	assert_eq!(entries[0].hash.as_str(), hash);
	// В каноничном выводе число и единица склеены: 0.02em
	let block = &entries[0].block;
	let seg = block
		.segments
		.iter()
		.find(|s| s.starts_with("letter-spacing:"))
		.expect("должен быть letter-spacing");
	assert_eq!(seg.trim(), "letter-spacing: 0.02em");
}

/// Строковая форма css!("...") позволяет писать 0.02em без пробела (обход бага с exponent).
#[test]
fn letter_spacing_0_02em_string_form_works() {
	let hash_block = css! {
		letter-spacing: 0.02 em;
	};
	let hash_string = css!("letter-spacing: 0.02em;");
	assert_eq!(
		hash_block, hash_string,
		"блочная форма (0.02 em) и строковая (0.02em) должны давать один хеш"
	);
}
