use blazing_css_core::{canonical_css_block_from_stream, hash_css_block, hash_css_body};
use proc_macro::{Literal, TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;

/// Expands to an encoded hash of the macro body.
/// Accepts either a block `css! { ... }` or a string literal `css!("...")`.
/// Use the string form for values that rustc parses as float exponent (e.g. `0.02em` → use `css!("letter-spacing: 0.02em;")` or `0.02 em` in block).
#[proc_macro]
pub fn css(input: TokenStream) -> TokenStream {
	let stream = TokenStream2::from(input.clone());
	// Если передан один строковый литерал — парсим тело как строку (обходит баг 0.02em в блоке).
	if let Ok(lit_str) = syn::parse2::<syn::LitStr>(stream) {
		let body = lit_str.value();
		let encoded = hash_css_body(&body);
		let literal = Literal::string(&encoded);
		return TokenStream::from(TokenTree::Literal(literal));
	}
	let stream = TokenStream2::from(input);
	let block = canonical_css_block_from_stream(&stream);
	let encoded = hash_css_block(&block);
	let literal = Literal::string(&encoded);
	TokenStream::from(TokenTree::Literal(literal))
}
