use proc_macro2::{Delimiter, TokenStream as TokenStream2, TokenTree};
use xxhash_rust::xxh32::xxh32;

const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

pub fn encode_hash(mut value: u32) -> String {
	let base = ALPHABET.len() as u32;
	if value == 0 {
		return (ALPHABET[0] as char).to_string();
	}

	let mut buffer = Vec::new();
	while value > 0 {
		buffer.push(ALPHABET[(value % base) as usize] as char);
		value /= base;
	}

	buffer.iter().rev().collect()
}

pub fn canonical_segments(body: &str) -> Vec<String> {
	body.split(';')
		.filter_map(|segment| {
			let trimmed = segment.trim();
			if trimmed.is_empty() {
				None
			} else {
				Some(normalize_segment(trimmed))
			}
		})
		.collect()
}

pub fn hash_css_body(body: &str) -> String {
	let canonical = canonical_segments(body).join(";");
	let hash = xxh32(canonical.as_bytes(), 0);
	encode_hash(hash)
}

fn normalize_segment(segment: &str) -> String {
	let mut result = String::new();
	let mut chars = segment.chars().peekable();

	while let Some(ch) = chars.next() {
		match ch {
			':' => {
				while result.ends_with(' ') {
					result.pop();
				}
				if !result.is_empty() {
					result.push(' ');
				}
				result.push(':');
				result.push(' ');
				while matches!(chars.peek(), Some(next) if next.is_whitespace()) {
					chars.next();
				}
			}
			c if c.is_whitespace() => {
				if !result.ends_with(' ') {
					result.push(' ');
				}
				while matches!(chars.peek(), Some(next) if next.is_whitespace()) {
					chars.next();
				}
			}
			_ => result.push(ch),
		}
	}

	result.trim().to_string()
}

pub fn canonical_segments_from_stream(stream: &TokenStream2) -> Vec<String> {
	let tokens = flatten_stream(stream);
	split_segments(&tokens)
		.into_iter()
		.filter_map(|segment| canonicalize_segment_tokens(&segment))
		.collect()
}

pub fn canonical_body_from_stream(stream: &TokenStream2) -> String {
	canonical_segments_from_stream(stream).join(";")
}

pub fn hash_css_segments(segments: &[String]) -> String {
	let canonical = segments.join(";");
	let hash = xxh32(canonical.as_bytes(), 0);
	encode_hash(hash)
}

#[derive(Clone, Debug)]
enum SimpleToken {
	Ident(String),
	Literal(String),
	Punct(char),
}

fn flatten_stream(stream: &TokenStream2) -> Vec<SimpleToken> {
	let mut tokens = Vec::new();
	for token in stream.clone() {
		flatten_token(token, &mut tokens);
	}
	tokens
}

fn flatten_token(token: TokenTree, out: &mut Vec<SimpleToken>) {
	match token {
		TokenTree::Ident(ident) => out.push(SimpleToken::Ident(ident.to_string())),
		TokenTree::Literal(literal) => out.push(SimpleToken::Literal(literal.to_string())),
		TokenTree::Punct(punct) => out.push(SimpleToken::Punct(punct.as_char())),
		TokenTree::Group(group) => match group.delimiter() {
			Delimiter::Parenthesis => {
				out.push(SimpleToken::Punct('('));
				for inner in group.stream() {
					flatten_token(inner, out);
				}
				out.push(SimpleToken::Punct(')'));
			}
			Delimiter::Bracket => {
				out.push(SimpleToken::Punct('['));
				for inner in group.stream() {
					flatten_token(inner, out);
				}
				out.push(SimpleToken::Punct(']'));
			}
			Delimiter::Brace => {
				out.push(SimpleToken::Punct('{'));
				for inner in group.stream() {
					flatten_token(inner, out);
				}
				out.push(SimpleToken::Punct('}'));
			}
			Delimiter::None => {
				for inner in group.stream() {
					flatten_token(inner, out);
				}
			}
		},
	}
}

fn split_segments(tokens: &[SimpleToken]) -> Vec<Vec<SimpleToken>> {
	let mut result = Vec::new();
	let mut current = Vec::new();
	let mut depth = DelimiterDepth::default();

	for token in tokens.iter().cloned() {
		if let SimpleToken::Punct(ch) = token {
			match ch {
				'(' => depth.paren += 1,
				')' => depth.paren = depth.paren.saturating_sub(1),
				'[' => depth.bracket += 1,
				']' => depth.bracket = depth.bracket.saturating_sub(1),
				'{' => depth.brace += 1,
				'}' => depth.brace = depth.brace.saturating_sub(1),
				';' if depth.is_zero() => {
					if !current.is_empty() {
						result.push(std::mem::take(&mut current));
					}
					continue;
				}
				_ => {}
			}
		}
		current.push(token);
	}

	if !current.is_empty() {
		result.push(current);
	}

	result
}

#[derive(Default)]
struct DelimiterDepth {
	paren: usize,
	bracket: usize,
	brace: usize,
}

impl DelimiterDepth {
	fn is_zero(&self) -> bool {
		self.paren == 0 && self.bracket == 0 && self.brace == 0
	}
}

fn canonicalize_segment_tokens(tokens: &[SimpleToken]) -> Option<String> {
	let (property_tokens, value_tokens) = split_property_and_value(tokens)?;
	let property = canonicalize_property(&property_tokens);
	if property.is_empty() {
		return None;
	}

	let value = canonicalize_value(&value_tokens);
	if value.is_empty() {
		Some(property)
	} else {
		Some(format!("{property}: {value}"))
	}
}

fn split_property_and_value(
	tokens: &[SimpleToken],
) -> Option<(Vec<SimpleToken>, Vec<SimpleToken>)> {
	let mut depth = DelimiterDepth::default();

	for (index, token) in tokens.iter().enumerate() {
		if let SimpleToken::Punct(ch) = token {
			match ch {
				'(' => depth.paren += 1,
				')' => depth.paren = depth.paren.saturating_sub(1),
				'[' => depth.bracket += 1,
				']' => depth.bracket = depth.bracket.saturating_sub(1),
				'{' => depth.brace += 1,
				'}' => depth.brace = depth.brace.saturating_sub(1),
				':' if depth.is_zero() => {
					let property = tokens[..index].to_vec();
					let value = tokens[index + 1..].to_vec();
					return Some((property, value));
				}
				_ => {}
			}
		}
	}

	None
}

fn canonicalize_property(tokens: &[SimpleToken]) -> String {
	let mut result = String::new();
	for token in tokens {
		match token {
			SimpleToken::Ident(text) | SimpleToken::Literal(text) => result.push_str(text),
			SimpleToken::Punct(ch) => result.push(*ch),
		}
	}
	result.trim().to_string()
}

fn canonicalize_value(tokens: &[SimpleToken]) -> String {
	if tokens.is_empty() {
		return String::new();
	}

	let pieces = tokenize_value(tokens);
	let mut writer = ValueWriter::new();
	for piece in pieces {
		writer.push(piece);
	}
	writer.finish()
}

fn tokenize_value(tokens: &[SimpleToken]) -> Vec<ValuePiece> {
	let mut pieces = Vec::new();
	let mut index = 0;
	let mut expect_value = true;

	while index < tokens.len() {
		match &tokens[index] {
			SimpleToken::Ident(_) => {
				let (text, next) = collect_ident_chain(tokens, index);
				let lower = text.to_ascii_lowercase();
				let kind = if is_css_unit(&lower) {
					ValueKind::Unit
				} else {
					ValueKind::Word
				};
				pieces.push(ValuePiece::new(text, kind));
				expect_value = false;
				index = next;
			}
			SimpleToken::Literal(text) => {
				if is_string_literal(text) {
					pieces.push(ValuePiece::new(
						literal_to_css_string(text),
						ValueKind::StringLiteral,
					));
				} else {
					pieces.push(ValuePiece::new(
						normalize_number_literal(text),
						ValueKind::Number,
					));
				}
				expect_value = false;
				index += 1;
			}
			SimpleToken::Punct(ch) => match ch {
				'(' => {
					pieces.push(ValuePiece::new("(".into(), ValueKind::OpenParen));
					expect_value = true;
					index += 1;
				}
				')' => {
					pieces.push(ValuePiece::new(")".into(), ValueKind::CloseParen));
					expect_value = false;
					index += 1;
				}
				'[' => {
					pieces.push(ValuePiece::new("[".into(), ValueKind::OpenParen));
					expect_value = true;
					index += 1;
				}
				']' => {
					pieces.push(ValuePiece::new("]".into(), ValueKind::CloseParen));
					expect_value = false;
					index += 1;
				}
				'{' => {
					pieces.push(ValuePiece::new("{".into(), ValueKind::OpenParen));
					expect_value = true;
					index += 1;
				}
				'}' => {
					pieces.push(ValuePiece::new("}".into(), ValueKind::CloseParen));
					expect_value = false;
					index += 1;
				}
				',' => {
					pieces.push(ValuePiece::new(",".into(), ValueKind::Comma));
					expect_value = true;
					index += 1;
				}
				'%' => {
					pieces.push(ValuePiece::new("%".into(), ValueKind::Percent));
					expect_value = false;
					index += 1;
				}
				'#' => {
					pieces.push(ValuePiece::new("#".into(), ValueKind::Hash));
					expect_value = false;
					index += 1;
				}
				'!' => {
					pieces.push(ValuePiece::new("!".into(), ValueKind::Bang));
					expect_value = true;
					index += 1;
				}
				'/' => {
					pieces.push(ValuePiece::new("/".into(), ValueKind::Slash));
					expect_value = true;
					index += 1;
				}
				'+' | '-' => {
					if expect_value {
						if let Some((piece, next)) = collect_prefixed_piece(tokens, index) {
							pieces.push(piece);
							index = next;
							expect_value = false;
						} else {
							pieces.push(ValuePiece::new(ch.to_string(), ValueKind::Operator));
							expect_value = true;
							index += 1;
						}
					} else {
						pieces.push(ValuePiece::new(ch.to_string(), ValueKind::Operator));
						expect_value = true;
						index += 1;
					}
				}
				':' => {
					pieces.push(ValuePiece::new(":".into(), ValueKind::Other));
					expect_value = true;
					index += 1;
				}
				';' => {
					index += 1;
				}
				'.' => {
					pieces.push(ValuePiece::new(".".into(), ValueKind::Other));
					expect_value = false;
					index += 1;
				}
				_ => {
					pieces.push(ValuePiece::new(ch.to_string(), ValueKind::Other));
					expect_value = false;
					index += 1;
				}
			},
		}
	}

	pieces
}

fn collect_ident_chain(tokens: &[SimpleToken], start: usize) -> (String, usize) {
	let mut text = String::new();
	let mut index = start;

	if let Some(SimpleToken::Ident(base)) = tokens.get(index) {
		text.push_str(base);
		index += 1;
		while index + 1 < tokens.len() {
			if let SimpleToken::Punct('-') = tokens[index] {
				if let SimpleToken::Ident(next) = &tokens[index + 1] {
					text.push('-');
					text.push_str(next);
					index += 2;
					continue;
				}
			}
			break;
		}
	}

	(text, index)
}

fn collect_prefixed_piece(tokens: &[SimpleToken], start: usize) -> Option<(ValuePiece, usize)> {
	let mut prefix = String::new();
	let mut index = start;

	while index < tokens.len() {
		match &tokens[index] {
			SimpleToken::Punct('-') | SimpleToken::Punct('+') => {
				prefix.push(match &tokens[index] {
					SimpleToken::Punct(ch) => *ch,
					_ => unreachable!(),
				});
				index += 1;
			}
			SimpleToken::Ident(_) => {
				let (ident, next) = collect_ident_chain(tokens, index);
				let mut text = prefix;
				text.push_str(&ident);
				let lower = text.to_ascii_lowercase();
				let kind = if is_css_unit(&lower) {
					ValueKind::Unit
				} else {
					ValueKind::Word
				};
				return Some((ValuePiece::new(text, kind), next));
			}
			SimpleToken::Literal(literal) => {
				if is_numeric_literal(literal) {
					let mut text = prefix;
					text.push_str(&normalize_number_literal(literal));
					return Some((ValuePiece::new(text, ValueKind::Number), index + 1));
				}
				break;
			}
			_ => break,
		}
	}

	None
}

fn is_string_literal(value: &str) -> bool {
	value.starts_with('"')
		|| value.starts_with('\'')
		|| value.starts_with("r\"")
		|| value.starts_with("r#")
}

fn normalize_number_literal(value: &str) -> String {
	value.chars().filter(|c| *c != '_').collect()
}

fn is_numeric_literal(value: &str) -> bool {
	value
		.chars()
		.all(|c| c.is_ascii_digit() || matches!(c, '.' | '_'))
}

fn literal_to_css_string(value: &str) -> String {
	if value.starts_with('"') || value.starts_with('\'') {
		value.to_string()
	} else if let Some(stripped) = value.strip_prefix('r') {
		let hash_count = stripped.chars().take_while(|c| *c == '#').count();
		let start = hash_count;
		let mut chars = stripped.chars().skip(start);
		if chars.next() == Some('"') {
			let suffix = format!("\"{}", "#".repeat(hash_count));
			if let Some(body) = stripped[start + 1..].strip_suffix(&suffix) {
				return format!("\"{body}\"");
			}
		}
		value.to_string()
	} else {
		value.to_string()
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ValueKind {
	Word,
	Number,
	StringLiteral,
	Unit,
	OpenParen,
	CloseParen,
	Comma,
	Percent,
	Operator,
	Slash,
	Hash,
	Bang,
	Other,
}

#[derive(Clone, Debug)]
struct ValuePiece {
	text: String,
	kind: ValueKind,
}

impl ValuePiece {
	fn new(text: String, kind: ValueKind) -> Self {
		Self { text, kind }
	}
}

struct ValueWriter {
	output: String,
	prev_kind: Option<ValueKind>,
}

impl ValueWriter {
	fn new() -> Self {
		Self {
			output: String::new(),
			prev_kind: None,
		}
	}

	fn push(&mut self, piece: ValuePiece) {
		match piece.kind {
			ValueKind::Comma => {
				self.trim_trailing_space();
				self.output.push(',');
				self.output.push(' ');
				self.prev_kind = Some(ValueKind::Comma);
			}
			ValueKind::OpenParen => {
				self.trim_trailing_space();
				self.output.push_str(&piece.text);
				self.prev_kind = Some(ValueKind::OpenParen);
			}
			ValueKind::CloseParen => {
				self.trim_trailing_space();
				self.output.push_str(&piece.text);
				self.prev_kind = Some(ValueKind::CloseParen);
			}
			ValueKind::Percent => {
				self.trim_trailing_space();
				self.output.push('%');
				self.prev_kind = Some(ValueKind::Percent);
			}
			ValueKind::Operator => {
				self.trim_trailing_space();
				if !self.output.is_empty() {
					self.output.push(' ');
				}
				self.output.push_str(&piece.text);
				self.output.push(' ');
				self.prev_kind = Some(ValueKind::Operator);
			}
			ValueKind::Slash => {
				self.trim_trailing_space();
				self.output.push('/');
				self.prev_kind = Some(ValueKind::Slash);
			}
			ValueKind::Bang => {
				if !self.output.is_empty() && !self.output.ends_with([' ', ':']) {
					self.output.push(' ');
				}
				self.output.push('!');
				self.prev_kind = Some(ValueKind::Bang);
			}
			ValueKind::Hash => {
				if matches!(
					self.prev_kind,
					Some(
						ValueKind::Word
							| ValueKind::Number | ValueKind::StringLiteral
							| ValueKind::Unit | ValueKind::CloseParen
							| ValueKind::Percent
					)
				) && !self.output.ends_with(' ')
				{
					self.output.push(' ');
				}
				self.output.push('#');
				self.prev_kind = Some(ValueKind::Hash);
			}
			ValueKind::Other => {
				self.trim_trailing_space();
				self.output.push_str(&piece.text);
				self.prev_kind = Some(ValueKind::Other);
			}
			ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral | ValueKind::Unit => {
				if self.needs_space(&piece) {
					self.output.push(' ');
				}
				self.output.push_str(&piece.text);
				self.prev_kind = Some(piece.kind);
			}
		}
	}

	fn needs_space(&self, piece: &ValuePiece) -> bool {
		match (self.prev_kind, piece.kind) {
			(None, _) => false,
			(Some(ValueKind::Comma), _) => false,
			(Some(ValueKind::Operator), _) => false,
			(_, ValueKind::Operator) => true,
			(Some(ValueKind::OpenParen), _) => false,
			(Some(ValueKind::Bang), _) => false,
			(Some(ValueKind::Hash), _) => false,
			(
				Some(ValueKind::Percent),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral | ValueKind::Unit,
			) => true,
			(
				Some(ValueKind::CloseParen),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral | ValueKind::Hash,
			) => true,
			(
				Some(ValueKind::Slash),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral,
			) => false,
			(
				Some(ValueKind::Word),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral | ValueKind::Unit,
			) => true,
			(Some(ValueKind::Number), ValueKind::Word) => true,
			(Some(ValueKind::Number), ValueKind::Unit) => false,
			(Some(ValueKind::Number), ValueKind::Number | ValueKind::StringLiteral) => true,
			(
				Some(ValueKind::Unit),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral,
			) => true,
			(
				Some(ValueKind::StringLiteral),
				ValueKind::Word | ValueKind::Number | ValueKind::StringLiteral | ValueKind::Unit,
			) => true,
			_ => false,
		}
	}

	fn trim_trailing_space(&mut self) {
		while self.output.ends_with(' ') {
			self.output.pop();
		}
	}

	fn finish(self) -> String {
		self.output.trim().to_string()
	}
}

fn is_css_unit(value: &str) -> bool {
	CSS_UNITS.contains(&value)
}

const CSS_UNITS: &[&str] = &[
	"cap", "ch", "cm", "deg", "dpcm", "dpi", "dppx", "dvh", "dvw", "em", "ex", "fr", "grad", "ic",
	"in", "khz", "hz", "lh", "lvh", "lvw", "mm", "ms", "pc", "pt", "px", "q", "rad", "rem", "rlh",
	"s", "svh", "svw", "turn", "vh", "vi", "vmax", "vmin", "vw",
];

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn canonicalizes_sample_css() {
		let stream: TokenStream2 = "display: grid; border-radius: 1rem; overflow: hidden; box-shadow: 0 12px 30px rgba(15, 15, 15, 0.35); justify-content: flex-start; width: 100%;"
			.parse()
			.unwrap();
		let segments = canonical_segments_from_stream(&stream);
		assert_eq!(segments, vec![
			"display: grid",
			"border-radius: 1rem",
			"overflow: hidden",
			"box-shadow: 0 12px 30px rgba(15, 15, 15, 0.35)",
			"justify-content: flex-start",
			"width: 100%"
		]);
	}

	#[test]
	fn preserves_calc_spacing() {
		let stream: TokenStream2 =
			"width: calc(100% - 2rem); transform: translateY(10px) rotate(45deg);"
				.parse()
				.unwrap();
		let segments = canonical_segments_from_stream(&stream);
		assert_eq!(segments, vec![
			"width: calc(100% - 2rem)",
			"transform: translateY(10px) rotate(45deg)"
		]);
	}
}
