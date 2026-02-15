use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use blazing_css_core::CssBlock;

use super::{format_css_block, process_file};

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
