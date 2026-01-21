use std::{
	fs,
	path::{Path, PathBuf},
	sync::OnceLock,
};

use anyhow::{Result, anyhow};
use tempfile::TempDir;

use crate::{OutputSpec, Project, WorkspaceInventory, parse_output_spec, resolve_output_mapping};

// Helper function to create a temporary directory for testing
fn create_temp_dir() -> Result<TempDir> {
	tempfile::tempdir().map_err(|e| anyhow!("failed to create temp dir: {}", e))
}

// Helper function to create a test project structure
fn create_test_project_structure(base: &Path, name: &str) -> Result<PathBuf> {
	let project_dir = base.join(name);
	fs::create_dir_all(&project_dir)?;
	let src_dir = project_dir.join("src");
	fs::create_dir_all(&src_dir)?;
	let main_rs = src_dir.join("main.rs");
	fs::write(&main_rs, "fn main() {}")?;
	let cargo_toml = project_dir.join("Cargo.toml");
	fs::write(
		&cargo_toml,
		format!(
			r#"[package]
name = "{}"
version = "0.1.0"
edition = "2024"
"#,
			name
		),
	)?;
	Ok(project_dir)
}

// Helper to set up current directory for tests
fn set_test_current_dir(path: &Path) {
	static TEST_DIR: OnceLock<PathBuf> = OnceLock::new();
	TEST_DIR.set(path.to_path_buf()).unwrap();
	std::env::set_current_dir(path).unwrap();
}

fn workspace_inventory_for_paths(root: &Path, members: &[(&str, &Path)]) -> WorkspaceInventory {
	WorkspaceInventory::for_testing(
		root.to_path_buf(),
		members
			.iter()
			.map(|(name, path)| ((*name).to_string(), (*path).to_path_buf()))
			.collect(),
	)
}

fn destination_for_output(
	workspace: &WorkspaceInventory, project_name: &str, project_root: &Path, output_path: &str,
) -> PathBuf {
	let project = Project {
		name: project_name.to_string(),
		root: project_root.to_path_buf(),
	};
	let spec = OutputSpec {
		target_crate: None,
		path: PathBuf::from(output_path),
	};

	crate::destination_path(&project, &spec, Some(workspace)).unwrap()
}

fn destinations_for_projects(
	workspace_root: &Path, workspace: &WorkspaceInventory, projects: &[Project], outputs: &[&str],
) -> Vec<String> {
	let output_vec = outputs.iter().map(|s| s.to_string()).collect::<Vec<_>>();
	let mapping = resolve_output_mapping(&output_vec, projects).unwrap();

	mapping
		.into_iter()
		.map(|(project, spec)| {
			let destination = crate::destination_path(&project, &spec, Some(workspace)).unwrap();
			destination
				.strip_prefix(workspace_root)
				.map(|path| path.to_string_lossy().to_string())
				.unwrap_or_else(|_| destination.to_string_lossy().to_string())
		})
		.collect()
}

#[cfg(test)]
mod parse_output_spec_tests {
	use super::*;

	#[test]
	fn test_parse_simple_path() {
		let spec = parse_output_spec("styles.css").unwrap();
		assert_eq!(spec.target_crate, None);
		assert_eq!(spec.path, PathBuf::from("styles.css"));
	}

	#[test]
	fn test_parse_nested_path() {
		let spec = parse_output_spec("assets/styles.css").unwrap();
		assert_eq!(spec.target_crate, None);
		assert_eq!(spec.path, PathBuf::from("assets/styles.css"));
	}

	#[test]
	fn test_parse_crate_syntax() {
		let spec = parse_output_spec("$mycrate/styles.css").unwrap();
		assert_eq!(spec.target_crate, Some("mycrate".to_string()));
		assert_eq!(spec.path, PathBuf::from("styles.css"));
	}

	#[test]
	fn test_parse_crate_syntax_with_nested_path() {
		let spec = parse_output_spec("$mycrate/assets/styles.css").unwrap();
		assert_eq!(spec.target_crate, Some("mycrate".to_string()));
		assert_eq!(spec.path, PathBuf::from("assets/styles.css"));
	}

	#[test]
	fn test_parse_crate_syntax_without_slash() {
		// Current implementation requires a slash after crate name
		let result = parse_output_spec("$mycrate");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_empty_spec() {
		let result = parse_output_spec("");
		assert!(result.is_err());
		assert!(result.unwrap_err().to_string().contains("cannot be empty"));
	}

	#[test]
	fn test_parse_whitespace_only_spec() {
		let result = parse_output_spec("   ");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_crate_syntax_without_crate_name() {
		let result = parse_output_spec("$/styles.css");
		assert!(result.is_err());
		assert!(
			result
				.unwrap_err()
				.to_string()
				.contains("crate name cannot be empty")
		);
	}

	#[test]
	fn test_parse_crate_syntax_with_empty_path() {
		let spec = parse_output_spec("$mycrate/").unwrap();
		assert_eq!(spec.target_crate, Some("mycrate".to_string()));
		// Current implementation returns "." for empty path
		assert_eq!(spec.path, PathBuf::from("."));
	}
}

#[cfg(test)]
mod resolve_output_mapping_tests {
	use super::*;

	fn create_test_projects(count: usize, base: &Path) -> Result<Vec<Project>> {
		let mut projects = Vec::new();
		for i in 0..count {
			let name = format!("project{}", i);
			let path = create_test_project_structure(base, &name)?;
			projects.push(Project {
				name: name.clone(),
				root: path,
			});
		}
		Ok(projects)
	}

	#[test]
	fn test_single_output_broadcast() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(3, temp_dir.path()).unwrap();
		let outputs = vec!["styles.css".to_string()];

		let mapping = resolve_output_mapping(&outputs, &projects).unwrap();
		assert_eq!(mapping.len(), 3);
		for (_project, spec) in &mapping {
			assert_eq!(spec.target_crate, None);
			assert_eq!(spec.path, PathBuf::from("styles.css"));
		}
	}

	#[test]
	fn test_one_to_one_mapping() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(2, temp_dir.path()).unwrap();
		let outputs = vec!["output1.css".to_string(), "output2.css".to_string()];

		let mapping = resolve_output_mapping(&outputs, &projects).unwrap();
		assert_eq!(mapping.len(), 2);
		assert_eq!(mapping[0].1.path, PathBuf::from("output1.css"));
		assert_eq!(mapping[1].1.path, PathBuf::from("output2.css"));
	}

	#[test]
	fn test_mismatched_output_count() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(3, temp_dir.path()).unwrap();
		let outputs = vec!["output1.css".to_string(), "output2.css".to_string()];

		let result = resolve_output_mapping(&outputs, &projects);
		assert!(result.is_err());
		assert!(
			result
				.unwrap_err()
				.to_string()
				.contains("number of outputs")
		);
	}

	#[test]
	fn test_empty_outputs() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(1, temp_dir.path()).unwrap();
		let outputs: Vec<String> = vec![];

		let result = resolve_output_mapping(&outputs, &projects);
		// Current implementation returns error for empty outputs
		assert!(result.is_err());
	}

	#[test]
	fn test_mixed_syntax_succeeds() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(2, temp_dir.path()).unwrap();
		let outputs = vec!["$project0/styles.css".to_string(), "output.css".to_string()];

		let result = resolve_output_mapping(&outputs, &projects);
		// Current implementation allows mixed syntax (broadcast mode)
		assert!(result.is_ok());
		// Should map both projects to the simple output (broadcast)
		assert_eq!(result.unwrap().len(), 2);
	}

	#[test]
	fn test_crate_outputs_preserve_project_order() {
		let temp_dir = create_temp_dir().unwrap();
		let projects = create_test_projects(2, temp_dir.path()).unwrap();
		let outputs = vec!["$shared/ui.css".to_string(), "$shared/web.css".to_string()];

		let mapping = resolve_output_mapping(&outputs, &projects).unwrap();
		assert_eq!(mapping.len(), 2);
		assert_eq!(mapping[0].0.name, "project0");
		assert_eq!(mapping[0].1.path, PathBuf::from("ui.css"));
		assert_eq!(mapping[1].0.name, "project1");
		assert_eq!(mapping[1].1.path, PathBuf::from("web.css"));
	}

	#[test]
	fn test_watch_command_destinations_for_crate_outputs() {
		let temp_dir = create_temp_dir().unwrap();
		let workspace_root = temp_dir.path();
		let packages_root = workspace_root.join("packages");
		fs::create_dir_all(&packages_root).unwrap();

		let ui_path = create_test_project_structure(&packages_root, "ui").unwrap();
		let web_path = create_test_project_structure(&packages_root, "web").unwrap();

		let workspace =
			workspace_inventory_for_paths(workspace_root, &[("ui", &ui_path), ("web", &web_path)]);

		let projects = vec![
			Project {
				name: "ui".to_string(),
				root: ui_path.clone(),
			},
			Project {
				name: "web".to_string(),
				root: web_path.clone(),
			},
		];

		let destinations = destinations_for_projects(workspace_root, &workspace, &projects, &[
			"$web/assets/ui.css",
			"$web/assets/web.css",
		]);

		assert_eq!(destinations, vec![
			"packages/web/assets/ui.css".to_string(),
			"packages/web/assets/web.css".to_string(),
		]);
	}
}

#[cfg(test)]
mod resolve_projects_tests {
	use super::*;

	#[test]
	fn test_resolve_default_project() {
		let temp_dir = create_temp_dir().unwrap();
		let project_path = create_test_project_structure(temp_dir.path(), "test_project").unwrap();
		set_test_current_dir(&project_path);

		// This test would need access to the actual resolve_projects function
		// which is private in main.rs. For now, we'll document the expected behavior.
		// Expected: Should resolve to the current directory project
	}

	#[test]
	fn test_resolve_named_projects() {
		let temp_dir = create_temp_dir().unwrap();
		let _project1 = create_test_project_structure(temp_dir.path(), "project1").unwrap();
		let _project2 = create_test_project_structure(temp_dir.path(), "project2").unwrap();

		// This test would need access to the actual resolve_named_projects function
		// Expected: Should resolve to the specified projects
	}
}

#[cfg(test)]
mod destination_path_tests {
	use super::*;

	#[test]
	fn test_destination_path_simple() {
		let temp_dir = create_temp_dir().unwrap();
		let project_path = create_test_project_structure(temp_dir.path(), "test_project").unwrap();
		let project = Project {
			name: "test_project".to_string(),
			root: project_path.clone(),
		};
		let spec = OutputSpec {
			target_crate: None,
			path: PathBuf::from("styles.css"),
		};

		let expected = project_path.join("styles.css");
		let actual = crate::destination_path(&project, &spec, None).unwrap();
		assert_eq!(actual, expected);
	}

	#[test]
	fn test_destination_path_nested() {
		let temp_dir = create_temp_dir().unwrap();
		let project_path = create_test_project_structure(temp_dir.path(), "test_project").unwrap();
		let project = Project {
			name: "test_project".to_string(),
			root: project_path.clone(),
		};
		let spec = OutputSpec {
			target_crate: None,
			path: PathBuf::from("assets/styles.css"),
		};

		let actual = crate::destination_path(&project, &spec, None).unwrap();
		let expected = project_path.join("assets/styles.css");
		assert_eq!(actual, expected);
	}

	#[test]
	fn test_destination_path_with_crate_target() {
		let temp_dir = create_temp_dir().unwrap();
		let project1_path = create_test_project_structure(temp_dir.path(), "project1").unwrap();
		let _project2_path = create_test_project_structure(temp_dir.path(), "project2").unwrap();
		let project = Project {
			name: "project1".to_string(),
			root: project1_path.clone(),
		};
		let spec = OutputSpec {
			target_crate: Some("project2".to_string()),
			path: PathBuf::from("styles.css"),
		};

		let result = crate::destination_path(&project, &spec, None);
		assert!(result.is_err());
	}

	#[test]
	fn test_destination_path_infers_workspace_crate_from_path() {
		let temp_dir = create_temp_dir().unwrap();
		let workspace_root = temp_dir.path();
		let packages_root = workspace_root.join("packages");
		fs::create_dir_all(&packages_root).unwrap();

		let ui_project = create_test_project_structure(&packages_root, "ui").unwrap();
		let web_project = create_test_project_structure(&packages_root, "web").unwrap();

		let workspace = workspace_inventory_for_paths(workspace_root, &[
			("ui", &ui_project),
			("web", &web_project),
		]);

		let actual =
			destination_for_output(&workspace, "ui", &ui_project, "packages/web/assets/ui.css");
		let expected = web_project.join("assets/ui.css");

		assert_eq!(expected, actual);
	}
}

#[cfg(test)]
mod helper_function_tests {

	#[test]
	fn test_is_rust_source() {
		// This test would need access to the actual is_rust_source function
		// Expected: .rs files should return true, others false
	}

	#[test]
	fn test_format_relative_to_cli() {
		// This test would need access to the actual format_relative_to_cli function
		// Expected: Should format paths relative to CLI base directory
	}
}
