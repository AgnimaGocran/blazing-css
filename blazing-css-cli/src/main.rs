use clap::Parser;

fn main() {
	let cli = blazing_css_cli::Cli::parse();
	if let Err(err) = blazing_css_cli::run(cli) {
		eprintln!("Error: {err}");
		std::process::exit(1);
	}
}
