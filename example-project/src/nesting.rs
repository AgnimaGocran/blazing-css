use blazing_css::css;

pub fn print_path() {
	// Nested pseudo-class and pseudo-elements (SCSS-style)
	println!("{}", css! {
		color: red;
		padding: 10px;

		&:hover {
			color: blue;
			background: yellow;
		}

		&::before {
			content: "";
			display: block;
		}

		&:focus {
			outline: none;

			&::after {
				content: "focused";
			}
		}
	});
}
