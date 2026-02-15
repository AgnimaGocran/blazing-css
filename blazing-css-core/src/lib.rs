use proc_macro2::{Delimiter, TokenStream as TokenStream2, TokenTree};
use xxhash_rust::xxh32::xxh32;

const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// Checks if a value is an array (starts with '[' and ends with ']')
pub fn is_array_value(value: &str) -> bool {
	let trimmed = value.trim();
	trimmed.starts_with('[') && trimmed.ends_with(']')
}

/// Parses an array value and returns the list of values inside brackets
/// Returns None if the value is not a valid array format
pub fn parse_array_value(value: &str) -> Option<Vec<String>> {
	let trimmed = value.trim();
	if !is_array_value(trimmed) {
		return None;
	}

	// Remove the outer brackets
	let inner = &trimmed[1..trimmed.len() - 1];
	if inner.is_empty() {
		return Some(Vec::new());
	}

	// Split by comma and trim each value
	let values: Vec<String> = inner
		.split(',')
		.map(|v| v.trim().to_string())
		.filter(|v| !v.is_empty())
		.collect();

	Some(values)
}

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
	let mut segments: Vec<String> = body
		.split(';')
		.filter_map(|segment| {
			let trimmed = segment.trim();
			if trimmed.is_empty() {
				None
			} else {
				Some(normalize_segment(trimmed))
			}
		})
		.collect();

	sort_segments_by_property(&mut segments);
	segments
}

pub fn hash_css_body(body: &str) -> String {
	let canonical = segments_to_canonical(&canonical_segments(body));
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

	let trimmed = result.trim();
	// Remove space before colons in array values like [value1, value2]
	trimmed.replace(" :", ":").replace(" ;", ";").to_string()
}

pub fn canonical_segments_from_stream(stream: &TokenStream2) -> Vec<String> {
	let tokens = flatten_stream(stream);
	let mut segments: Vec<String> = split_segments(&tokens)
		.into_iter()
		.filter_map(|segment| canonicalize_segment_tokens(&segment))
		.collect();

	sort_segments_by_property(&mut segments);
	segments
}

fn sort_segments_by_property(segments: &mut [String]) {
	segments.sort_by(|left, right| segment_property(left).cmp(segment_property(right)));
}

fn segment_property(segment: &str) -> &str {
	if let Some((property, _)) = segment.split_once(':') {
		property.trim()
	} else {
		segment.trim()
	}
}

pub fn canonical_body_from_stream(stream: &TokenStream2) -> String {
	canonical_segments_from_stream(stream).join(";")
}

pub fn hash_css_segments(segments: &[String]) -> String {
	let canonical = segments_to_canonical(segments);
	let hash = xxh32(canonical.as_bytes(), 0);
	encode_hash(hash)
}

/// A CSS block with optional nested selector blocks (e.g. pseudo-classes, pseudo-elements).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CssBlock {
	/// Selector suffix relative to parent (e.g. ":hover", "::before"). Empty for root block.
	pub selector_suffix: String,
	/// CSS properties in this block: "property: value"
	pub segments: Vec<String>,
	/// Nested blocks (e.g. &:hover { ... })
	pub children: Vec<CssBlock>,
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

#[derive(Clone, Debug)]
enum TopLevelItem {
	Segment(Vec<SimpleToken>),
	NestedBlock {
		selector: Vec<SimpleToken>,
		body: Vec<SimpleToken>,
	},
}

/// Splits token stream into top-level segments (property: value;) and nested blocks (selector { ... }).
fn split_into_items(tokens: &[SimpleToken]) -> Vec<TopLevelItem> {
	let mut result = Vec::new();
	let mut current = Vec::new();
	let mut depth = DelimiterDepth::default();
	let mut i = 0;

	while i < tokens.len() {
		let token = tokens[i].clone();

		if let SimpleToken::Punct(ch) = &token {
			match ch {
				'(' => {
					depth.paren += 1;
					current.push(token);
					i += 1;
				}
				')' => {
					depth.paren = depth.paren.saturating_sub(1);
					current.push(token);
					i += 1;
				}
				'[' => {
					depth.bracket += 1;
					current.push(token);
					i += 1;
				}
				']' => {
					depth.bracket = depth.bracket.saturating_sub(1);
					current.push(token);
					i += 1;
				}
				'{' if depth.is_zero() => {
					let selector = std::mem::take(&mut current);
					depth.brace += 1;
					i += 1;
					let start = i;
					while i < tokens.len() {
						match &tokens[i] {
							SimpleToken::Punct('{') => depth.brace += 1,
							SimpleToken::Punct('}') => {
								depth.brace = depth.brace.saturating_sub(1);
								if depth.brace == 0 {
									let body = tokens[start..i].to_vec();
									i += 1;
									result.push(TopLevelItem::NestedBlock { selector, body });
									break;
								}
							}
							_ => {}
						}
						i += 1;
					}
				}
				'{' => {
					depth.brace += 1;
					current.push(token);
					i += 1;
				}
				'}' => {
					depth.brace = depth.brace.saturating_sub(1);
					current.push(token);
					i += 1;
				}
				';' if depth.is_zero() => {
					if !current.is_empty() {
						result.push(TopLevelItem::Segment(std::mem::take(&mut current)));
					}
					i += 1;
				}
				_ => {
					current.push(token);
					i += 1;
				}
			}
		} else {
			current.push(token);
			i += 1;
		}
	}

	if !current.is_empty() {
		result.push(TopLevelItem::Segment(current));
	}

	result
}

/// Canonicalizes selector tokens (e.g. & : hover -> ":hover", & : : before -> "::before").
fn canonicalize_selector(tokens: &[SimpleToken]) -> String {
	let mut out = String::new();
	let mut prev_is_ident_or_literal = false;

	for token in tokens {
		match token {
			SimpleToken::Ident(s) => {
				// Пробел между двумя словами: "@keyframes spin", но не "fade" + "-" + "in"
				if prev_is_ident_or_literal {
					out.push(' ');
				}
				out.push_str(s);
				prev_is_ident_or_literal = true;
			}
			SimpleToken::Literal(s) => {
				if prev_is_ident_or_literal {
					out.push(' ');
				}
				out.push_str(s);
				prev_is_ident_or_literal = true;
			}
			SimpleToken::Punct(c) => {
				// Пунктуация приклеивается без пробелов: @, :, -, (, ) и т.д.
				out.push(*c);
				prev_is_ident_or_literal = false;
			}
		}
	}

	let trimmed = out.trim().trim_start_matches('&').trim().to_string();
	trimmed
}

/// Parses a slice of tokens into a CssBlock (root or nested).
fn parse_css_block(tokens: &[SimpleToken]) -> CssBlock {
	let items = split_into_items(tokens);
	let mut segments: Vec<String> = Vec::new();
	let mut children: Vec<CssBlock> = Vec::new();

	for item in items {
		match item {
			TopLevelItem::Segment(seg_tokens) => {
				if let Some(seg) = canonicalize_segment_tokens(&seg_tokens) {
					segments.push(seg);
				}
			}
			TopLevelItem::NestedBlock { selector, body } => {
				let selector_suffix = canonicalize_selector(&selector);
				let child = parse_css_block(&body);
				children.push(CssBlock {
					selector_suffix,
					segments: child.segments,
					children: child.children,
				});
			}
		}
	}

	sort_segments_by_property(&mut segments);

	CssBlock {
		selector_suffix: String::new(),
		segments,
		children,
	}
}

/// Builds a canonical CssBlock from the macro token stream (supports nesting).
pub fn canonical_css_block_from_stream(stream: &TokenStream2) -> CssBlock {
	let tokens = flatten_stream(stream);
	parse_css_block(&tokens)
}

/// Serializes a CssBlock into a canonical CSS string for hashing.
///
/// For a flat block (no children) this produces the same string as
/// `segments_to_canonical` — i.e. `hash_css_block` and `hash_css_segments`
/// yield the same hash when there is no nesting.
///
/// For nested blocks the output looks like readable CSS:
///   `display: flex; flex-direction: row; &:hover { background: red; }`
fn block_to_canonical_string(block: &CssBlock) -> String {
	let mut out = segments_to_canonical(&block.segments);

	let mut child_strs: Vec<String> = block
		.children
		.iter()
		.map(|child| {
			let inner = block_to_canonical_string(child);
			let prefix = if child.selector_suffix.starts_with(':')
				|| child.selector_suffix.starts_with("::") {
				"&"
			} else {
				""
			};
			format!("{prefix}{} {{ {inner} }}", child.selector_suffix)
		})
		.collect();
	child_strs.sort();

	for child_str in child_strs {
		if !out.is_empty() {
			out.push(' ');
		}
		out.push_str(&child_str);
	}

	out
}

/// Canonical representation of segments: "prop: val; prop: val;"
fn segments_to_canonical(segments: &[String]) -> String {
	if segments.is_empty() {
		return String::new();
	}
	let mut out = String::new();
	for seg in segments {
		if !out.is_empty() {
			out.push(' ');
		}
		out.push_str(seg);
		out.push(';');
	}
	out
}

/// Returns a hash string for the whole block tree (same content => same hash).
pub fn hash_css_block(block: &CssBlock) -> String {
	let canonical = block_to_canonical_string(block);
	let hash = xxh32(canonical.as_bytes(), 0);
	encode_hash(hash)
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
			"border-radius: 1rem",
			"box-shadow: 0 12px 30px rgba(15, 15, 15, 0.35)",
			"display: grid",
			"justify-content: flex-start",
			"overflow: hidden",
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
			"transform: translateY(10px) rotate(45deg)",
			"width: calc(100% - 2rem)"
		]);
	}

	#[test]
	fn hashes_equal_for_different_attribute_order() {
		let a: TokenStream2 = "display: grid; width: 100%; gap: 1rem;".parse().unwrap();
		let b: TokenStream2 = "gap: 1rem; display: grid; width: 100%;".parse().unwrap();

		let segments_a = canonical_segments_from_stream(&a);
		let segments_b = canonical_segments_from_stream(&b);

		assert_eq!(segments_a, segments_b);
		assert_eq!(
			hash_css_segments(&segments_a),
			hash_css_segments(&segments_b)
		);
	}

	#[test]
	fn flat_block_matches_segments() {
		let stream: TokenStream2 = "color: red; padding: 10px;".parse().unwrap();
		let block = canonical_css_block_from_stream(&stream);
		assert!(block.selector_suffix.is_empty());
		assert!(block.children.is_empty());
		let expected = canonical_segments_from_stream(&stream);
		assert_eq!(block.segments, expected);
	}

	#[test]
	fn parses_nested_pseudo_class() {
		let stream: TokenStream2 = r#"color: red; &:hover { color: blue; background: yellow; }"#
			.parse()
			.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		assert_eq!(block.segments, vec!["color: red"]);
		assert_eq!(block.children.len(), 1);
		let child = &block.children[0];
		assert_eq!(child.selector_suffix, ":hover");
		assert_eq!(child.segments, vec!["background: yellow", "color: blue"]);
		assert!(child.children.is_empty());
	}

	#[test]
	fn parses_nested_pseudo_element() {
		let stream: TokenStream2 = r#"&::before { content: ""; display: block; }"#
			.parse()
			.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		assert!(block.segments.is_empty());
		assert_eq!(block.children.len(), 1);
		assert_eq!(block.children[0].selector_suffix, "::before");
		assert_eq!(
			block.children[0].segments,
			vec!["content: \"\"", "display: block"]
		);
	}

	#[test]
	fn parses_deeply_nested() {
		let stream: TokenStream2 = r#"
			outline: none;
			&:focus {
				outline: none;
				&::after { content: "focused"; }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		assert_eq!(block.segments, vec!["outline: none"]);
		assert_eq!(block.children.len(), 1);
		let focus = &block.children[0];
		assert_eq!(focus.selector_suffix, ":focus");
		assert_eq!(focus.segments, vec!["outline: none"]);
		assert_eq!(focus.children.len(), 1);
		assert_eq!(focus.children[0].selector_suffix, "::after");
		assert_eq!(focus.children[0].segments, vec!["content: \"focused\""]);
	}

	#[test]
	fn hash_css_block_deterministic() {
		let stream: TokenStream2 = "color: red; &:hover { color: blue; }".parse().unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let h1 = hash_css_block(&block);
		let h2 = hash_css_block(&block);
		assert_eq!(h1, h2);
	}

	#[test]
	fn parses_block_with_animation_and_keyframes() {
		let stream: TokenStream2 = r#"
			position: absolute;
			top: 0.25rem;
			right: 0.35rem;
			width: 6px;
			height: 6px;
			border-radius: 999px;
			background: rgb(234, 179, 8);
			animation: engine-pulse 0.8s ease-in-out infinite alternate;
			@keyframes engine-pulse {
				from { opacity: 0.3; }
				to { opacity: 1.0; }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);

		assert_eq!(block.segments, vec![
			"animation: engine-pulse 0.8s ease-in-out infinite alternate",
			"background: rgb(234, 179, 8)",
			"border-radius: 999px",
			"height: 6px",
			"position: absolute",
			"right: 0.35rem",
			"top: 0.25rem",
			"width: 6px"
		]);

		assert_eq!(block.children.len(), 1);
		let keyframes = &block.children[0];
		assert!(keyframes.selector_suffix.contains("keyframes"));
		assert!(keyframes.selector_suffix.contains("engine"));
		assert!(keyframes.selector_suffix.contains("pulse"));
		assert!(keyframes.segments.is_empty());
		assert_eq!(keyframes.children.len(), 2);

		assert_eq!(keyframes.children[0].selector_suffix, "from");
		assert_eq!(keyframes.children[0].segments, vec!["opacity: 0.3"]);
		assert_eq!(keyframes.children[1].selector_suffix, "to");
		assert_eq!(keyframes.children[1].segments, vec!["opacity: 1.0"]);

		let compact: TokenStream2 = "position:absolute;top:0.25rem;right:0.35rem;width:6px;height:6px;border-radius:999px;background:rgb(234,179,8);animation:engine-pulse 0.8s ease-in-out infinite alternate;@keyframes engine-pulse{from{opacity:0.3;}to{opacity:1.0;}}"
			.parse()
			.unwrap();
		let compact_block = canonical_css_block_from_stream(&compact);
		assert_eq!(hash_css_block(&block), hash_css_block(&compact_block));
	}

	#[test]
	fn hash_css_block_equals_hash_css_segments_for_flat_blocks() {
		let stream: TokenStream2 = "display: grid; justify-content: center;"
			.parse()
			.unwrap();
		let segments = canonical_segments_from_stream(&stream);
		let block = canonical_css_block_from_stream(&stream);
		assert_eq!(
			hash_css_segments(&segments),
			hash_css_block(&block),
			"flat block hash must match segments hash"
		);
	}

	#[test]
	fn canonical_string_for_hover_block() {
		let stream: TokenStream2 = r#"
			color: red;
			padding: 10px;
			&:hover { background: blue; }
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		assert_eq!(
			canonical,
			"color: red; padding: 10px; &:hover { background: blue; }"
		);
	}

	#[test]
	fn canonical_string_for_keyframes_block() {
		let stream: TokenStream2 = r#"
			animation: spin 1s linear infinite;
			@keyframes spin {
				from { transform: rotate(0deg); }
				to { transform: rotate(360deg); }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		assert_eq!(
			canonical,
			"animation: spin 1s linear infinite; @keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }"
		);
	}

	#[test]
	fn canonical_string_for_media_block() {
		let stream: TokenStream2 = r#"
			display: grid;
			gap: 1rem;
			@media (max-width: 900px) {
				display: flex;
				flex-direction: column;
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		assert_eq!(
			canonical,
			"display: grid; gap: 1rem; @media(max-width:900px) { display: flex; flex-direction: column; }"
		);
	}

	#[test]
	fn canonical_string_for_deeply_nested() {
		let stream: TokenStream2 = r#"
			outline: none;
			&:focus {
				outline: none;
				&::after { content: "focused"; }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		assert_eq!(
			canonical,
			"outline: none; &:focus { outline: none; &::after { content: \"focused\"; } }"
		);
	}

	#[test]
	fn different_nesting_produces_different_hashes() {
		let a: TokenStream2 = "color: red; &:hover { color: blue; }".parse().unwrap();
		let b: TokenStream2 = "color: red; &:focus { color: blue; }".parse().unwrap();
		let c: TokenStream2 = "color: red;".parse().unwrap();

		let ha = hash_css_block(&canonical_css_block_from_stream(&a));
		let hb = hash_css_block(&canonical_css_block_from_stream(&b));
		let hc = hash_css_block(&canonical_css_block_from_stream(&c));

		assert_ne!(ha, hb, "hover vs focus must differ");
		assert_ne!(ha, hc, "with nesting vs without must differ");
		assert_ne!(hb, hc, "with nesting vs without must differ");
	}

	#[test]
	fn keyframes_canonical_simple() {
		let stream: TokenStream2 = r#"
			@keyframes fade-in {
				from { opacity: 0; }
				to { opacity: 1; }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		assert_eq!(
			canonical,
			"@keyframes fade-in { from { opacity: 0; } to { opacity: 1; } }"
		);
	}

	#[test]
	fn keyframes_canonical_with_percentages() {
		let stream: TokenStream2 = r#"
			animation: pulse 2s ease infinite;
			@keyframes pulse {
				0% { transform: scale(1); }
				50% { transform: scale(1.1); }
				100% { transform: scale(1); }
			}
		"#
		.parse()
		.unwrap();
		let block = canonical_css_block_from_stream(&stream);
		let canonical = block_to_canonical_string(&block);
		// дочерние блоки сортируются лексикографически
		assert!(canonical.starts_with("animation: pulse 2s ease infinite; "));
		assert!(canonical.contains("@keyframes pulse {"));
		assert!(canonical.contains("0% { transform: scale(1); }"));
		assert!(canonical.contains("50% { transform: scale(1.1); }"));
		assert!(canonical.contains("100% { transform: scale(1); }"));
	}

	#[test]
	fn keyframes_hash_same_regardless_of_formatting() {
		let a: TokenStream2 = r#"
			animation: spin 1s linear infinite;
			@keyframes spin {
				from { transform: rotate(0deg); }
				to   { transform: rotate(360deg); }
			}
		"#
		.parse()
		.unwrap();

		let b: TokenStream2 =
			"animation:spin 1s linear infinite;@keyframes spin{from{transform:rotate(0deg);}to{transform:rotate(360deg);}}"
			.parse()
			.unwrap();

		assert_eq!(
			hash_css_block(&canonical_css_block_from_stream(&a)),
			hash_css_block(&canonical_css_block_from_stream(&b)),
		);
	}

	#[test]
	fn keyframes_hash_differs_for_different_animation_name() {
		let a: TokenStream2 = r#"
			animation: spin 1s linear infinite;
			@keyframes spin {
				from { opacity: 0; }
				to { opacity: 1; }
			}
		"#
		.parse()
		.unwrap();

		let b: TokenStream2 = r#"
			animation: fade 1s linear infinite;
			@keyframes fade {
				from { opacity: 0; }
				to { opacity: 1; }
			}
		"#
		.parse()
		.unwrap();

		assert_ne!(
			hash_css_block(&canonical_css_block_from_stream(&a)),
			hash_css_block(&canonical_css_block_from_stream(&b)),
			"different keyframes names must produce different hashes"
		);
	}

	#[test]
	fn keyframes_hash_differs_for_different_steps() {
		let a: TokenStream2 = r#"
			@keyframes pulse {
				from { opacity: 0.3; }
				to { opacity: 1.0; }
			}
		"#
		.parse()
		.unwrap();

		let b: TokenStream2 = r#"
			@keyframes pulse {
				from { opacity: 0; }
				to { opacity: 1.0; }
			}
		"#
		.parse()
		.unwrap();

		assert_ne!(
			hash_css_block(&canonical_css_block_from_stream(&a)),
			hash_css_block(&canonical_css_block_from_stream(&b)),
			"different keyframe step values must produce different hashes"
		);
	}

	#[test]
	fn keyframes_hash_differs_with_and_without_keyframes() {
		let with_kf: TokenStream2 = r#"
			animation: spin 1s linear infinite;
			@keyframes spin {
				from { opacity: 0; }
				to { opacity: 1; }
			}
		"#
		.parse()
		.unwrap();

		let without_kf: TokenStream2 = "animation: spin 1s linear infinite;"
			.parse()
			.unwrap();

		assert_ne!(
			hash_css_block(&canonical_css_block_from_stream(&with_kf)),
			hash_css_block(&canonical_css_block_from_stream(&without_kf)),
			"block with keyframes must differ from block without"
		);
	}

	#[test]
	fn keyframes_hash_same_regardless_of_step_order() {
		// from/to порядок не должен влиять — дочерние блоки сортируются
		let a: TokenStream2 = r#"
			@keyframes blink {
				from { opacity: 0; }
				to { opacity: 1; }
			}
		"#
		.parse()
		.unwrap();

		let b: TokenStream2 = r#"
			@keyframes blink {
				to { opacity: 1; }
				from { opacity: 0; }
			}
		"#
		.parse()
		.unwrap();

		assert_eq!(
			hash_css_block(&canonical_css_block_from_stream(&a)),
			hash_css_block(&canonical_css_block_from_stream(&b)),
			"keyframe step order should not affect hash"
		);
	}
}
