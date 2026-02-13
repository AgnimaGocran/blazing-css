use blazing_css_core::{canonical_css_block_from_stream, hash_css_block};
use proc_macro::{Literal, TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;

/// Expands to an encoded hash of the macro body.
#[proc_macro]
pub fn css(input: TokenStream) -> TokenStream {
	let stream = TokenStream2::from(input);
	let block = canonical_css_block_from_stream(&stream);
	let encoded = hash_css_block(&block);
	let literal = Literal::string(&encoded);
	TokenStream::from(TokenTree::Literal(literal))
}
