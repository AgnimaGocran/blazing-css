use blazing_css::css;

pub fn print_path() {
	// Example 1: Array values for responsive flex-direction
	// With break-points=500,1000,2000 this will generate media queries:
	// - <=500px: row
	// - <=1000px: column
	// - <=2000px: column-reverse
	// - >2000px: column-reverse (fallback)
	println!("{}", css! {
		display: flex;
		flex-direction: [row, column, column-reverse];
	});

	// Example 2: Array values with fewer values than breakpoints
	// The last value will be repeated for remaining breakpoints
	println!("{}", css! {
		display: flex;
		justify-content: [flex-start, flex-end];
	});

	// Example 3: Regular CSS without array values (no media queries)
	// This will always generate a simple CSS rule regardless of break-points
	println!("{}", css! {
		display: block;
		color: blue;
	});

	// Example 4: Mixed - some properties with arrays, some without
	println!("{}", css! {
		display: grid;
		grid-template-columns: [1fr, 2fr, 3fr];
		gap: 10px;
	});
}
