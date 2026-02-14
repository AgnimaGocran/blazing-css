use std::{
	collections::{HashMap, HashSet},
	env,
	ffi::OsString,
	fmt, fs, io,
	path::{Path, PathBuf},
	sync::{
		OnceLock,
		mpsc::{Sender, channel},
	},
};

use anyhow::{Context, Result, anyhow};
use blazing_css::{RenderOptions, render_css_with_options_in};
use cargo_metadata::{
	Error as CargoMetadataError, Metadata, MetadataCommand, PackageId, camino::Utf8PathBuf,
};
use clap::{Args, Parser, Subcommand};
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher, event::EventKind};
use owo_colors::OwoColorize;

const BZC_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser, Debug)]
#[command(
	name = "bzc",
	version,
	about = "Watch/render blazing-css styles for Rust projects"
)]
pub struct Cli {
	#[command(subcommand)]
	pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
	/// Run a single pass of CSS generation
	Render(CommandArgs),
	/// Render first, then watch for file changes
	Watch(CommandArgs),
}

#[derive(Args, Debug, Clone)]
pub struct CommandArgs {
	/// Explicit set of project names (comma separated or repeated)
	#[arg(short = 'p', long, value_name = "NAME", value_delimiter = ',')]
	pub project: Vec<String>,
	/// Output CSS path(s) relative to each project root (comma separated for multiple outputs)
	#[arg(value_name = "OUTPUT", value_delimiter = ',')]
	pub output: Vec<String>,
	/// Breakpoints for responsive design (comma separated, e.g., 500,1000,2000)
	#[arg(long, value_name = "PX", value_delimiter = ',')]
	pub break_points: Option<Vec<u32>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Project {
	pub name: String,
	pub root: PathBuf,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OutputSpec {
	pub target_crate: Option<String>,
	pub path: PathBuf,
}

pub fn parse_output_spec(spec: &str) -> Result<OutputSpec> {
	let spec = spec.trim();
	if spec.is_empty() {
		return Err(anyhow!("output spec cannot be empty"));
	}

	if let Some(first) = spec.chars().next() {
		if first == '$' || first == '@' {
			return parse_crate_spec(spec, first);
		}
	}

	// Simple path without target crate
	Ok(OutputSpec {
		target_crate: None,
		path: PathBuf::from(spec),
	})
}

fn parse_crate_spec(spec: &str, marker: char) -> Result<OutputSpec> {
	let rest = &spec[1..];
	let slash_idx = rest.find('/').ok_or_else(|| {
		anyhow!(
			"invalid {}crate syntax: expected {}crate/path, got {}{}",
			marker,
			marker,
			marker,
			rest
		)
	})?;

	let crate_name = &rest[..slash_idx];
	if crate_name.is_empty() {
		return Err(anyhow!("crate name cannot be empty in {}{}", marker, rest));
	}

	let path_str = &rest[slash_idx + 1..];
	let path = if path_str.is_empty() {
		PathBuf::from(".")
	} else {
		PathBuf::from(path_str)
	};

	Ok(OutputSpec {
		target_crate: Some(crate_name.to_string()),
		path,
	})
}

pub fn resolve_output_mapping(
	outputs: &[String], projects: &[Project],
) -> Result<Vec<(Project, OutputSpec)>> {
	let output_specs: Vec<OutputSpec> = outputs
		.iter()
		.map(|s| parse_output_spec(s))
		.collect::<Result<_>>()?;

	if output_specs.is_empty() {
		return Err(anyhow!("at least one output must be specified"));
	}

	match output_specs.len() {
		1 => {
			// Single output for all projects (broadcast)
			let spec = output_specs.into_iter().next().unwrap();
			Ok(projects
				.iter()
				.cloned()
				.map(|project| (project, spec.clone()))
				.collect())
		}
		n if n == projects.len() => {
			// 1:1 mapping by order
			Ok(projects
				.iter()
				.cloned()
				.zip(output_specs.into_iter())
				.collect())
		}
		n => Err(anyhow!(
			"number of outputs ({}) must be 1 or equal to number of projects ({}), got {}",
			n,
			projects.len(),
			n
		)),
	}
}

pub fn run(args: Cli) -> Result<()> {
	log_startup_version();

	let workspace = match WorkspaceInventory::load() {
		Ok(ws) => Some(ws),
		Err(err) => {
			log_warn(format!("unable to read workspace metadata: {err}"));
			None
		}
	};

	match args.command {
		Command::Render(args) => run_render(&args, workspace.as_ref()),
		Command::Watch(args) => run_watch(&args, workspace.as_ref()),
	}
}

fn run_render(args: &CommandArgs, workspace: Option<&WorkspaceInventory>) -> Result<()> {
	let projects = resolve_projects(args, workspace)?;
	let output_mapping = resolve_output_mapping(&args.output, &projects)?;

	for (project, output_spec) in &output_mapping {
		render_project(project, output_spec, args.break_points.clone(), workspace)?;
	}
	Ok(())
}

fn run_watch(args: &CommandArgs, workspace: Option<&WorkspaceInventory>) -> Result<()> {
	let projects = resolve_projects(args, workspace)?;
	let output_mapping = resolve_output_mapping(&args.output, &projects)?;

	for (project, output_spec) in &output_mapping {
		render_project(project, output_spec, args.break_points.clone(), workspace)?;
	}
	watch_projects(output_mapping, args.break_points.clone(), workspace)
}

fn render_project(
	project: &Project, output_spec: &OutputSpec, break_points: Option<Vec<u32>>,
	workspace: Option<&WorkspaceInventory>,
) -> Result<()> {
	let destination = destination_path(project, output_spec, workspace)?;
	if let Some(parent) = destination.parent() {
		fs::create_dir_all(parent)
			.with_context(|| format!("failed to create output directory {}", parent.display()))?;
	}

	let arg_value = output_argument(output_spec, &destination);
	log_action(format!(
		"generating CSS for {} -> {}",
		project.name,
		format_relative_to_cli(&destination)
	));
	let options = RenderOptions {
		emit_cargo_directives: false,
		break_points,
	};
	render_css_with_options_in(&arg_value, &project.root, options)
		.map_err(|err| anyhow!("failed to render CSS for {}: {err}", project.name))?;
	Ok(())
}

fn watch_projects(
	output_mapping: Vec<(Project, OutputSpec)>, break_points: Option<Vec<u32>>,
	workspace: Option<&WorkspaceInventory>,
) -> Result<()> {
	let (tx, rx) = channel::<WatchMessage>();
	let mut watchers = Vec::new();

	// Collect unique projects from output_mapping for watching
	let mut unique_projects: Vec<Project> = Vec::new();
	let mut project_to_output: HashMap<String, Vec<OutputSpec>> = HashMap::new();
	for (project, output_spec) in output_mapping {
		project_to_output
			.entry(project.name.clone())
			.or_insert_with(Vec::new)
			.push(output_spec);
		if !unique_projects.iter().any(|p| p.name == project.name) {
			unique_projects.push(project);
		}
	}

	for (idx, project) in unique_projects.iter().enumerate() {
		let mut watcher =
			RecommendedWatcher::new(create_event_handler(idx, tx.clone()), Config::default())
				.context("failed to initialize file watcher")?;

		let src = project.root.join("src");
		let watch_path = if src.exists() {
			src
		} else {
			project.root.clone()
		};
		if !watch_path.exists() {
			return Err(anyhow!(
				"cannot watch project {} at {}; path does not exist",
				project.name,
				watch_path.display()
			));
		}

		log_watch(format!(
			"watching {} ({})",
			project.name,
			format_relative_to_cli(&watch_path)
		));
		watcher
			.watch(&watch_path, RecursiveMode::Recursive)
			.with_context(|| format!("failed to watch {}", watch_path.display()))?;
		watchers.push(watcher);
	}

	drop(tx);

	while let Ok(message) = rx.recv() {
		match message {
			WatchMessage::Event { idx, event } => {
				let rust_paths = rust_paths(&event);
				if rust_paths.is_empty() {
					continue;
				}

				if let Some(project) = unique_projects.get(idx) {
					if let Some(output_specs) = project_to_output.get(&project.name) {
						for output_spec in output_specs {
							log_changed_paths(project, &rust_paths);
							if let Err(err) = render_project(
								project,
								output_spec,
								break_points.clone(),
								workspace,
							) {
								log_error(format!("failed to rebuild {}: {err:#}", project.name));
							}
						}
					}
				}
			}
			WatchMessage::Error { idx, error } => {
				if let Some(project) = unique_projects.get(idx) {
					log_error(format!("watcher error for {}: {error}", project.name));
				} else {
					log_error(format!("watcher error: {error}"));
				}
			}
		}
	}

	drop(watchers);
	Ok(())
}

fn resolve_projects(
	args: &CommandArgs, workspace: Option<&WorkspaceInventory>,
) -> Result<Vec<Project>> {
	let projects = if args.project.is_empty() {
		vec![resolve_default_project(workspace)?]
	} else {
		resolve_named_projects(&args.project, workspace)?
	};

	if projects.is_empty() {
		return Err(anyhow!("no projects resolved"));
	}

	Ok(projects)
}

fn create_event_handler(
	idx: usize, tx: Sender<WatchMessage>,
) -> impl Fn(Result<Event, notify::Error>) {
	move |res| match res {
		Ok(event) => {
			let _ = tx.send(WatchMessage::Event { idx, event });
		}
		Err(error) => {
			let _ = tx.send(WatchMessage::Error { idx, error });
		}
	}
}

#[derive(Debug)]
enum WatchMessage {
	Event { idx: usize, event: Event },
	Error { idx: usize, error: notify::Error },
}

fn rust_paths(event: &Event) -> Vec<PathBuf> {
	match event.kind {
		EventKind::Access(_) | EventKind::Other => return Vec::new(),
		_ => {}
	}

	event
		.paths
		.iter()
		.filter(|path| is_rust_source(path))
		.cloned()
		.collect()
}

pub fn is_rust_source(path: &Path) -> bool {
	path.extension().is_some_and(|ext| ext == "rs")
}

fn log_changed_paths(project: &Project, paths: &[PathBuf]) {
	if paths.is_empty() {
		return;
	}

	let rendered = paths
		.iter()
		.map(|path| format_relative_to_cli(path))
		.collect::<Vec<_>>()
		.join(", ");
	log_change(format!("change detected in {}: {}", project.name, rendered));
}

pub fn format_relative_to_cli(path: &Path) -> String {
	let base = cli_base_dir();
	if let Ok(stripped) = path.strip_prefix(base) {
		let text = stripped.display().to_string();
		if text.is_empty() {
			".".to_string()
		} else {
			text
		}
	} else {
		path.display().to_string()
	}
}

fn cli_base_dir() -> &'static PathBuf {
	static START_DIR: OnceLock<PathBuf> = OnceLock::new();
	START_DIR.get_or_init(|| env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
}

fn log_action(message: impl fmt::Display) {
	log_message(LogTarget::Stdout, LogKind::Action, message);
}

fn log_startup_version() {
	log_action(format!("bzc v{BZC_VERSION}"));
}

fn log_watch(message: impl fmt::Display) {
	log_message(LogTarget::Stdout, LogKind::Watch, message);
}

fn log_change(message: impl fmt::Display) {
	log_message(LogTarget::Stdout, LogKind::Change, message);
}

fn log_warn(message: impl fmt::Display) {
	log_message(LogTarget::Stderr, LogKind::Warning, message);
}

fn log_error(message: impl fmt::Display) {
	log_message(LogTarget::Stderr, LogKind::Error, message);
}

fn log_message(target: LogTarget, kind: LogKind, message: impl fmt::Display) {
	let tag = "[bzc]".bold().cyan().to_string();
	let icon = kind.style_icon();
	let text = kind.style_text(message.to_string());
	match target {
		LogTarget::Stdout => println!("{} {} {}", tag, icon, text),
		LogTarget::Stderr => eprintln!("{} {} {}", tag, icon, text),
	}
}

#[derive(Clone, Copy)]
enum LogTarget {
	Stdout,
	Stderr,
}

#[derive(Clone, Copy)]
enum LogKind {
	Action,
	Watch,
	Change,
	Warning,
	Error,
}

impl LogKind {
	fn style_icon(self) -> String {
		match self {
			LogKind::Action => "⚡".bright_green().to_string(),
			LogKind::Watch => "👀".bright_blue().to_string(),
			LogKind::Change => "✏".yellow().to_string(),
			LogKind::Warning => "⚠".magenta().to_string(),
			LogKind::Error => "✖".bright_red().to_string(),
		}
	}

	fn style_text(self, text: String) -> String {
		match self {
			LogKind::Action => text.bright_green().to_string(),
			LogKind::Watch => text.bright_blue().to_string(),
			LogKind::Change => text.yellow().to_string(),
			LogKind::Warning => text.magenta().to_string(),
			LogKind::Error => text.bright_red().to_string(),
		}
	}
}

fn resolve_named_projects(
	names: &[String], workspace: Option<&WorkspaceInventory>,
) -> Result<Vec<Project>> {
	let cwd = env::current_dir().context("failed to read current directory")?;
	let mut resolved = Vec::new();
	for name in names {
		let trimmed = name.trim();
		if trimmed.is_empty() {
			return Err(anyhow!("project names cannot be empty"));
		}

		if let Some(path) = workspace.and_then(|ws| ws.path_by_name(trimmed)) {
			resolved.push(Project {
				name: trimmed.to_string(),
				root: path,
			});
			continue;
		}

		let fallback = cwd.join(trimmed);
		if fallback.exists() {
			resolved.push(Project {
				name: trimmed.to_string(),
				root: fallback,
			});
		} else {
			return Err(anyhow!(
				"project `{}` was not found in workspace or current directory",
				trimmed
			));
		}
	}
	Ok(resolved)
}

fn resolve_default_project(workspace: Option<&WorkspaceInventory>) -> Result<Project> {
	let cwd = env::current_dir().context("failed to read current directory")?;
	let canonical = canonicalize(&cwd);
	let name = workspace
		.and_then(|ws| ws.name_by_path(&canonical))
		.map(|s| s.to_string())
		.or_else(|| {
			cwd.file_name()
				.and_then(|os| os.to_str())
				.map(|s| s.to_string())
		})
		.unwrap_or_else(|| "current".to_string());

	Ok(Project { name, root: cwd })
}

struct WorkspaceInventory {
	workspace_root: PathBuf,
	by_name: HashMap<String, PathBuf>,
	by_path: HashMap<PathBuf, String>,
}

impl WorkspaceInventory {
	fn load() -> Result<Self> {
		let metadata =
			Self::load_metadata_with_fallback().context("failed to invoke cargo metadata")?;
		Self::from_metadata(metadata)
	}

	fn load_metadata_with_fallback() -> std::result::Result<Metadata, CargoMetadataError> {
		let mut command = MetadataCommand::new();
		command.no_deps();

		match command.exec() {
			Ok(metadata) => Ok(metadata),
			Err(err) if is_missing_cargo(&err) => {
				let mut fallback = MetadataCommand::new();
				fallback.cargo_path("cargo").no_deps();
				fallback.exec()
			}
			Err(err) => Err(err),
		}
	}

	fn from_metadata(metadata: Metadata) -> Result<Self> {
		let WorkspaceMembers {
			workspace_root,
			by_name,
			by_path,
		} = collect_workspace_members(&metadata)?;

		Ok(Self {
			workspace_root,
			by_name,
			by_path,
		})
	}

	fn path_by_name(&self, name: &str) -> Option<PathBuf> {
		self.by_name.get(name).cloned()
	}

	fn name_by_path(&self, path: &Path) -> Option<&str> {
		let canonical = canonicalize(path);
		self.by_path.get(&canonical).map(String::as_str)
	}

	#[cfg(test)]
	pub(crate) fn for_testing(workspace_root: PathBuf, members: Vec<(String, PathBuf)>) -> Self {
		let mut by_name = HashMap::new();
		let mut by_path = HashMap::new();
		for (name, path) in members {
			let canonical = canonicalize(&path);
			by_name.insert(name.clone(), canonical.clone());
			by_path.insert(canonical, name);
		}

		Self {
			workspace_root: canonicalize(&workspace_root),
			by_name,
			by_path,
		}
	}

	fn absolute_path_from_workspace(&self, path: &Path) -> PathBuf {
		if path.is_absolute() {
			path.to_path_buf()
		} else {
			self.workspace_root.join(path)
		}
	}

	fn infer_workspace_member_path(&self, path: &Path) -> Option<PathBuf> {
		let absolute = self.absolute_path_from_workspace(path);

		if self.is_workspace_member_path(&absolute) {
			return Some(absolute);
		}

		self.normalize_absolute_path(&absolute)
			.filter(|normalized| self.is_workspace_member_path(normalized))
	}

	fn is_workspace_member_path(&self, path: &Path) -> bool {
		self.by_path.keys().any(|root| path.starts_with(root))
	}

	fn normalize_absolute_path(&self, path: &Path) -> Option<PathBuf> {
		match path.canonicalize() {
			Ok(canon) => Some(canon),
			Err(_) => {
				let mut pending: Vec<OsString> = Vec::new();
				let mut current = path;

				while let Some(parent) = current.parent() {
					if let Some(name) = current.file_name() {
						pending.push(name.to_os_string());
					}

					match parent.canonicalize() {
						Ok(mut canon_parent) => {
							for component in pending.iter().rev() {
								canon_parent.push(component);
							}
							return Some(canon_parent);
						}
						Err(_) => {
							current = parent;
							continue;
						}
					}
				}

				None
			}
		}
	}
}

fn is_missing_cargo(err: &CargoMetadataError) -> bool {
	match err {
		CargoMetadataError::Io(io_err) => io_err.kind() == io::ErrorKind::NotFound,
		_ => false,
	}
}

struct WorkspaceMembers {
	workspace_root: PathBuf,
	by_name: HashMap<String, PathBuf>,
	by_path: HashMap<PathBuf, String>,
}

fn collect_workspace_members(metadata: &Metadata) -> Result<WorkspaceMembers> {
	let member_ids: HashSet<PackageId> = metadata.workspace_members.iter().cloned().collect();
	let mut by_name = HashMap::new();
	let mut by_path = HashMap::new();
	let workspace_root = canonicalize(metadata.workspace_root.as_std_path());

	for package in &metadata.packages {
		if !member_ids.contains(&package.id) {
			continue;
		}

		let manifest_dir = manifest_dir(&package.manifest_path);
		by_name.insert(package.name.clone(), manifest_dir.clone());
		by_path.insert(canonicalize(&manifest_dir), package.name.clone());
	}

	Ok(WorkspaceMembers {
		workspace_root,
		by_name,
		by_path,
	})
}

fn manifest_dir(path: &Utf8PathBuf) -> PathBuf {
	let manifest = path.clone().into_std_path_buf();
	manifest
		.parent()
		.map(|parent| parent.to_path_buf())
		.unwrap_or(manifest)
}

fn canonicalize(path: &Path) -> PathBuf {
	path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

pub(crate) fn destination_path(
	project: &Project, output_spec: &OutputSpec, workspace: Option<&WorkspaceInventory>,
) -> Result<PathBuf> {
	let base = if let Some(ref crate_name) = output_spec.target_crate {
		let ws = workspace.ok_or_else(|| {
			anyhow!(
				"workspace metadata is required to resolve crate target '{}'",
				crate_name
			)
		})?;

		ws.path_by_name(crate_name)
			.ok_or_else(|| anyhow!("crate '{}' not found in workspace", crate_name))?
	} else if let Some(ws) = workspace {
		if let Some(path) = ws.infer_workspace_member_path(&output_spec.path) {
			return Ok(path);
		}
		project.root.clone()
	} else {
		project.root.clone()
	};

	// Handle absolute paths that may have been expanded from shell variables
	if output_spec.path.is_absolute() {
		// If the absolute path is within the base directory, make it relative
		if let Ok(relative) = output_spec.path.strip_prefix(&base) {
			Ok(base.join(relative))
		} else if output_spec.path.starts_with("/") {
			// Handle paths like "/assets/file.css" (from empty shell variables)
			// by stripping the leading "/" and making it relative to base
			let path_str = output_spec.path.to_string_lossy();
			let relative_path = path_str.strip_prefix('/').unwrap_or(&path_str);
			Ok(base.join(relative_path))
		} else {
			// Keep absolute path if it's outside the base directory
			Ok(output_spec.path.clone())
		}
	} else {
		Ok(base.join(&output_spec.path))
	}
}

fn output_argument(_output_spec: &OutputSpec, destination: &Path) -> String {
	// Always return the absolute destination path for rendering
	destination.to_string_lossy().to_string()
}

#[cfg(test)]
mod tests;
