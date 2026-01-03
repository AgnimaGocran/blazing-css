mod probe;

use blazing_css::css;

fn main() {
	println!("{}1", css! {
		color: red;
		padding: 33px;
	});
	probe::print_path();
}
