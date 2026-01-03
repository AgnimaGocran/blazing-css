use blazing_css_core::hash_css_body;
use proc_macro::{Literal, TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;

/// Expands to an encoded hash of the macro body.
#[proc_macro]
pub fn css(input: TokenStream) -> TokenStream {
    let css_body = TokenStream2::from(input).to_string();
    let encoded = hash_css_body(&css_body);
    let literal = Literal::string(&encoded);
    TokenStream::from(TokenTree::Literal(literal))
}
