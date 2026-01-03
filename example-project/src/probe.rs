use blazing_css::css;

pub fn print_path() {
	println!("{}", css! {
		background: blue;
		margin: 12px 0;
	});
}
