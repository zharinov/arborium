// =============================================================================
// Grammar tracking and updates
// =============================================================================

/// Grammar configuration from GRAMMARS.toml
#[derive(Debug)]
#[allow(dead_code)]
struct GrammarConfig {
    name: String,
    repo: String,
    commit: String,
    license: String,
}

/// Parse GRAMMARS.toml
/// TODO: remove, we should only use info.toml
fn parse_grammars_toml(
    repo_root: &Path,
) -> Result<BTreeMap<String, GrammarConfig>, Box<dyn std::error::Error>> {
    let path = repo_root.join("GRAMMARS.toml");
    let contents = fs::read_to_string(&path)?;

    let mut grammars = BTreeMap::new();
    let mut current_name: Option<String> = None;
    let mut current_repo: Option<String> = None;
    let mut current_commit: Option<String> = None;
    let mut current_license: Option<String> = None;

    for line in contents.lines() {
        let line = line.trim();

        // Skip comments and empty lines
        if line.starts_with('#') || line.is_empty() {
            continue;
        }

        // Section header [name]
        if line.starts_with('[') && line.ends_with(']') {
            // Save previous grammar if any
            if let (Some(name), Some(repo), Some(commit), Some(license)) = (
                current_name.take(),
                current_repo.take(),
                current_commit.take(),
                current_license.take(),
            ) {
                grammars.insert(
                    name.clone(),
                    GrammarConfig {
                        name,
                        repo,
                        commit,
                        license,
                    },
                );
            }
            current_name = Some(line[1..line.len() - 1].to_string());
            continue;
        }

        // Key = value
        if let Some((key, value)) = line.split_once('=') {
            let key = key.trim();
            let value = value.trim().trim_matches('"');

            match key {
                "repo" => current_repo = Some(value.to_string()),
                "commit" => current_commit = Some(value.to_string()),
                "license" => current_license = Some(value.to_string()),
                _ => {}
            }
        }
    }

    // Save last grammar
    if let (Some(name), Some(repo), Some(commit), Some(license)) =
        (current_name, current_repo, current_commit, current_license)
    {
        grammars.insert(
            name.clone(),
            GrammarConfig {
                name,
                repo,
                commit,
                license,
            },
        );
    }

    Ok(grammars)
}

/// Get the latest commit hash from a remote repository
fn get_remote_head(repo_url: &str) -> Result<String, Box<dyn std::error::Error>> {
    if repo_url == "local" {
        return Ok("local".to_string());
    }

    let output = Command::new("git")
        .args(["ls-remote", repo_url, "HEAD"])
        .output()?;

    if !output.status.success() {
        return Err(format!("Failed to query {}", repo_url).into());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let commit = stdout
        .split_whitespace()
        .next()
        .ok_or_else(|| format!("No commit found for {}", repo_url))?;

    Ok(commit.to_string())
}

/// Check for updates to vendored grammars
fn check_updates() {
    let repo_root = find_repo_root().expect("Could not find repo root");
    println!("Checking for grammar updates...\n");

    let grammars = match parse_grammars_toml(&repo_root) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Error parsing GRAMMARS.toml: {}", e);
            std::process::exit(1);
        }
    };

    let mut updates_available = Vec::new();
    let mut up_to_date = 0;
    let mut errors = 0;

    for (name, config) in &grammars {
        if config.repo == "local" {
            println!("  {} (local, skipping)", name);
            continue;
        }

        print!("  {} ... ", name);

        match get_remote_head(&config.repo) {
            Ok(remote_commit) => {
                if remote_commit == config.commit {
                    println!("up to date");
                    up_to_date += 1;
                } else {
                    println!("UPDATE AVAILABLE");
                    println!(
                        "    current: {}",
                        &config.commit[..12.min(config.commit.len())]
                    );
                    println!(
                        "    latest:  {}",
                        &remote_commit[..12.min(remote_commit.len())]
                    );
                    updates_available.push((name.clone(), config.commit.clone(), remote_commit));
                }
            }
            Err(e) => {
                println!("ERROR: {}", e);
                errors += 1;
            }
        }
    }

    println!();
    println!("Summary:");
    println!("  {} up to date", up_to_date);
    println!("  {} updates available", updates_available.len());
    if errors > 0 {
        println!("  {} errors", errors);
    }

    if !updates_available.is_empty() {
        println!();
        println!("To update a grammar, run:");
        for (name, _, _) in &updates_available {
            println!("  cargo xtask vendor {}", name);
        }
    }
}

/// Vendor (update) a specific grammar
fn vendor_grammar(name: &str) {
    let repo_root = find_repo_root().expect("Could not find repo root");

    let grammars = match parse_grammars_toml(&repo_root) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Error parsing GRAMMARS.toml: {}", e);
            std::process::exit(1);
        }
    };

    let config = match grammars.get(name) {
        Some(c) => c,
        None => {
            eprintln!("Unknown grammar: {}", name);
            eprintln!("Available grammars:");
            for name in grammars.keys() {
                eprintln!("  {}", name);
            }
            std::process::exit(1);
        }
    };

    if config.repo == "local" {
        eprintln!("Cannot vendor local grammar: {}", name);
        std::process::exit(1);
    }

    println!("Vendoring {} from {}", name, config.repo);

    // Determine the target directory name
    let dir_name = format!("tree-sitter-{}", name);
    let target_dir = repo_root.join("grammars").join(&dir_name);

    // Clone to temp directory
    let temp_dir = std::env::temp_dir().join(format!("arborium-vendor-{}", name));
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir).expect("Failed to clean temp dir");
    }

    println!("  Cloning {}...", config.repo);
    let status = Command::new("git")
        .args([
            "clone",
            "--depth",
            "1",
            &config.repo,
            temp_dir.to_str().unwrap(),
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .status()
        .expect("Failed to run git clone");

    if !status.success() {
        eprintln!("Failed to clone repository");
        std::process::exit(1);
    }

    // Get the new commit hash
    let new_commit = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to get commit hash");

    let new_commit = String::from_utf8_lossy(&new_commit.stdout)
        .trim()
        .to_string();
    println!("  New commit: {}", &new_commit[..12.min(new_commit.len())]);

    // Create target directory structure
    if target_dir.exists() {
        // Preserve queries directory if it exists and has local modifications
        let queries_backup = if target_dir.join("queries").exists() {
            let backup = std::env::temp_dir().join(format!("arborium-queries-backup-{}", name));
            if backup.exists() {
                fs::remove_dir_all(&backup).ok();
            }
            fs::rename(target_dir.join("queries"), &backup).ok();
            Some(backup)
        } else {
            None
        };

        fs::remove_dir_all(&target_dir).expect("Failed to remove old grammar dir");
        fs::create_dir_all(&target_dir).expect("Failed to create grammar dir");

        // Restore queries if we backed them up
        if let Some(backup) = queries_backup {
            fs::rename(&backup, target_dir.join("queries")).ok();
        }
    } else {
        fs::create_dir_all(&target_dir).expect("Failed to create grammar dir");
    }

    // Copy essential files
    println!("  Copying files...");

    // Copy LICENSE
    for license_file in &["LICENSE", "LICENSE.md", "COPYING.txt", "COPYING"] {
        let src = temp_dir.join(license_file);
        if src.exists() {
            fs::copy(&src, target_dir.join(license_file)).ok();
            break;
        }
    }

    // Determine the source directory for grammar files
    // Check info.toml for subdir field (for multi-grammar repos like tree-sitter-xml)
    let crate_info_path = repo_root
        .join("crates")
        .join(format!("arborium-{}", name))
        .join("info.toml");
    let subdir = if crate_info_path.exists() {
        fs::read_to_string(&crate_info_path)
            .ok()
            .and_then(|content| {
                for line in content.lines() {
                    let line = line.trim();
                    if line.starts_with("subdir") {
                        if let Some(value) = line.split('=').nth(1) {
                            // Strip inline comments first, then trim whitespace and quotes
                            let value = value.split('#').next().unwrap_or(value);
                            let value = value.trim().trim_matches('"').trim_matches('\'');
                            if !value.is_empty() {
                                return Some(value.to_string());
                            }
                        }
                    }
                }
                None
            })
    } else {
        None
    };

    let grammar_source_dir = if let Some(ref subdir) = subdir {
        println!("  (multi-grammar repo, using {}/)", subdir);
        temp_dir.join(subdir)
    } else {
        temp_dir.clone()
    };

    // Copy grammar.js
    // For multi-grammar repos, fix import paths (e.g., ../common/ -> ./common/)
    let grammar_js = grammar_source_dir.join("grammar.js");
    if grammar_js.exists() {
        if subdir.is_some() {
            // Fix import paths when copying from subdir
            let content = fs::read_to_string(&grammar_js).expect("Failed to read grammar.js");
            let fixed = content
                .replace("'../common/", "'./common/")
                .replace("\"../common/", "\"./common/");
            fs::write(target_dir.join("grammar.js"), fixed).expect("Failed to write grammar.js");
        } else {
            fs::copy(&grammar_js, target_dir.join("grammar.js"))
                .expect("Failed to copy grammar.js");
        }
    }

    // Copy package.json if it exists (some grammars have npm dependencies)
    // Try grammar-specific first, then root
    let package_json = if grammar_source_dir.join("package.json").exists() {
        grammar_source_dir.join("package.json")
    } else {
        temp_dir.join("package.json")
    };
    if package_json.exists() {
        fs::copy(&package_json, target_dir.join("package.json")).ok();
    }

    // Copy grammar/ directory if it exists (for grammars with multiple files)
    let grammar_dir = grammar_source_dir.join("grammar");
    if grammar_dir.exists() {
        copy_dir_recursive(&grammar_dir, &target_dir.join("grammar"))
            .expect("Failed to copy grammar/");
    }

    // Copy additional directories that some grammars need for their grammar.js
    // Check both root level (for shared dirs like common/) and grammar-specific
    for extra_dir in &["lib", "common", "rules"] {
        // First try root level (shared directories)
        let root_extra = temp_dir.join(extra_dir);
        if root_extra.exists() {
            copy_dir_recursive(&root_extra, &target_dir.join(extra_dir))
                .unwrap_or_else(|e| eprintln!("  Warning: Failed to copy {}/: {}", extra_dir, e));
        }
        // Then try grammar-specific (may override)
        if grammar_source_dir != temp_dir {
            let grammar_extra = grammar_source_dir.join(extra_dir);
            if grammar_extra.exists() {
                copy_dir_recursive(&grammar_extra, &target_dir.join(extra_dir)).unwrap_or_else(
                    |e| eprintln!("  Warning: Failed to copy {}/: {}", extra_dir, e),
                );
            }
        }
    }

    // Copy all root-level JS files (helper files that grammars may need)
    for source_dir in &[&temp_dir, &grammar_source_dir] {
        if let Ok(entries) = fs::read_dir(source_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "js" || ext == "mjs" {
                            let filename = path.file_name().unwrap();
                            // grammar.js is already copied separately
                            if filename != "grammar.js" {
                                fs::copy(&path, target_dir.join(filename)).ok();
                            }
                        }
                    }
                }
            }
        }
    }

    // Copy src/ directory
    let src_dir = grammar_source_dir.join("src");
    if src_dir.exists() {
        copy_dir_recursive(&src_dir, &target_dir.join("src")).expect("Failed to copy src/");
    }

    // Copy queries/ - check grammar-specific first, then root, if target doesn't have them
    if !target_dir.join("queries").exists() {
        let queries_dir = if grammar_source_dir.join("queries").exists() {
            grammar_source_dir.join("queries")
        } else {
            temp_dir.join("queries")
        };
        if queries_dir.exists() {
            copy_dir_recursive(&queries_dir, &target_dir.join("queries"))
                .expect("Failed to copy queries/");
        }
    }

    // Clean up temp dir
    fs::remove_dir_all(&temp_dir).ok();

    // Update GRAMMARS.toml
    println!("  Updating GRAMMARS.toml...");
    let grammars_path = repo_root.join("GRAMMARS.toml");
    let contents = fs::read_to_string(&grammars_path).expect("Failed to read GRAMMARS.toml");

    // Replace the commit line for this grammar

    // We need to be careful to only replace within the right section
    let mut new_contents = String::new();
    let mut in_target_section = false;
    let mut replaced = false;

    for line in contents.lines() {
        let trimmed = line.trim();

        // Check for section header
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let section_name = &trimmed[1..trimmed.len() - 1];
            in_target_section = section_name == name;
        }

        // Replace commit line if in target section
        if in_target_section && trimmed.starts_with("commit = ") && !replaced {
            new_contents.push_str(&line.replace(&config.commit, &new_commit));
            replaced = true;
        } else {
            new_contents.push_str(line);
        }
        new_contents.push('\n');
    }

    fs::write(&grammars_path, new_contents).expect("Failed to write GRAMMARS.toml");

    println!();
    println!(
        "Done! Grammar {} updated to {}",
        name,
        &new_commit[..12.min(new_commit.len())]
    );
    println!();
    println!("Next steps:");
    println!("  1. Run: cargo xtask regenerate");
    println!("  2. Run: cargo build");
    println!("  3. Run: cargo test");
}

fn clean_grammar(grammar_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let mut removed_count = 0;

    // Clean root directory
    for entry in fs::read_dir(grammar_dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = path.file_name().unwrap().to_string_lossy();

        if path.is_file() {
            // Keep explicitly listed files and all .js/.mjs files (helper modules)
            let dominated = KEEP_FILES.iter().any(|&f| name == f)
                || name.ends_with(".js")
                || name.ends_with(".mjs");
            if !dominated {
                fs::remove_file(&path)?;
                removed_count += 1;
            }
        } else if path.is_dir() {
            if !KEEP_DIRS.iter().any(|&d| name == d) {
                fs::remove_dir_all(&path)?;
                removed_count += 1;
            }
        }
    }

    // Clean src/ directory
    let src_dir = grammar_dir.join("src");
    if src_dir.exists() {
        for entry in fs::read_dir(&src_dir)? {
            let entry = entry?;
            let path = entry.path();
            let name = path.file_name().unwrap().to_string_lossy();

            if path.is_file() {
                // Keep files with allowed extensions
                let has_allowed_ext = KEEP_SRC_EXTENSIONS.iter().any(|ext| name.ends_with(ext));
                if !has_allowed_ext {
                    fs::remove_file(&path)?;
                    removed_count += 1;
                }
            } else if path.is_dir() {
                if !KEEP_SRC_DIRS.iter().any(|&d| name == d) {
                    fs::remove_dir_all(&path)?;
                    removed_count += 1;
                }
            }
        }
    }

    let _ = removed_count; // suppress unused warning
    Ok(())
}

fn regenerate_grammar(grammar_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Set NODE_PATH to grammars/ dir so grammars can find their dependencies
    // grammar_dir is grammars/tree-sitter-*, so parent is grammars/
    let grammars_dir = grammar_dir.parent().unwrap_or(grammar_dir);

    let output = Command::new("tree-sitter")
        .args(["generate"])
        .current_dir(grammar_dir)
        .env("NODE_PATH", grammars_dir)
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Print full error to stderr for debugging
        eprintln!(
            "\n{} {} failed:\n{}",
            "Error:".red().bold(),
            grammar_dir.file_name().unwrap().to_string_lossy(),
            stderr
        );

        // Return a short summary for the results table
        let error_msg = stderr
            .lines()
            .find(|l| {
                l.contains("Cannot find module") || l.contains("error") || l.contains("Error")
            })
            .unwrap_or("generation failed")
            .trim();
        return Err(error_msg.to_string().into());
    }

    Ok(())
}

fn init_grammar(grammar_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Only run init --update if tree-sitter.json already exists
    // (otherwise it prompts interactively)
    let config_file = grammar_dir.join("tree-sitter.json");
    if config_file.exists() {
        let status = Command::new("tree-sitter")
            .args(["init", "--update"])
            .current_dir(grammar_dir)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()?;

        if !status.success() {
            return Err(format!(
                "tree-sitter init --update failed for {}",
                grammar_dir.display()
            )
            .into());
        }
    }

    // Run pnpm install if package.json exists and has dependencies
    let package_json = grammar_dir.join("package.json");
    if package_json.exists() {
        // Check if node_modules already exists
        let node_modules = grammar_dir.join("node_modules");
        if !node_modules.exists() {
            let _ = Command::new("pnpm")
                .args(["install", "--ignore-scripts"])
                .current_dir(grammar_dir)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            // Non-fatal - some grammars may not need npm deps
        }
    }

    Ok(())
}

/// Set up node_modules symlinks so grammars can find their dependencies
fn setup_node_modules(repo_root: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let grammars_dir = repo_root.join("grammars");
    let node_modules = grammars_dir.join("node_modules");

    // Create node_modules if it doesn't exist
    if !node_modules.exists() {
        fs::create_dir(&node_modules)?;
        println!("  Created grammars/node_modules/");
    }

    // Create symlinks for each grammar that is a dependency
    let mut deps_needed: HashSet<&str> = HashSet::new();
    for (_, grammar_deps) in GRAMMAR_DEPS {
        for dep in *grammar_deps {
            deps_needed.insert(dep);
        }
    }

    for dep in deps_needed {
        let target = grammars_dir.join(dep);
        let link = node_modules.join(dep);

        if target.exists() {
            // Remove existing symlink if present
            if link.exists() || link.symlink_metadata().is_ok() {
                fs::remove_file(&link).or_else(|_| fs::remove_dir_all(&link))?;
            }

            #[cfg(unix)]
            std::os::unix::fs::symlink(&target, &link)?;
            #[cfg(windows)]
            std::os::windows::fs::symlink_dir(&target, &link)?;

            println!("  Symlinked {} -> {}", dep, target.display());
        } else {
            println!(
                "  Warning: dependency {} not found at {}",
                dep,
                target.display()
            );
        }
    }

    Ok(())
}

fn find_grammars(repo_root: &Path) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut grammars = Vec::new();
    let grammars_dir = repo_root.join("grammars");

    for entry in fs::read_dir(&grammars_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            let name = path.file_name().unwrap().to_string_lossy();
            if name.starts_with("tree-sitter-") {
                // Verify it has a grammar.js in root
                if path.join("grammar.js").exists() {
                    grammars.push(path);
                } else {
                    // Check for multi-grammar repos (e.g., tree-sitter-typescript has typescript/ and tsx/ subdirs)
                    for subentry in fs::read_dir(&path).into_iter().flatten().flatten() {
                        let subpath = subentry.path();
                        if subpath.is_dir() && subpath.join("grammar.js").exists() {
                            // This is a sub-grammar, add it as a separate grammar
                            grammars.push(subpath);
                        }
                    }
                    // Also check for grammars/ subdirectory pattern (e.g., tree-sitter-ocaml/grammars/ocaml/)
                    let nested_grammars = path.join("grammars");
                    if nested_grammars.is_dir() {
                        for subentry in fs::read_dir(&nested_grammars)
                            .into_iter()
                            .flatten()
                            .flatten()
                        {
                            let subpath = subentry.path();
                            if subpath.is_dir() && subpath.join("grammar.js").exists() {
                                grammars.push(subpath);
                            }
                        }
                    }
                }
            }
        }
    }

    grammars.sort();
    println!("  Found {} grammars:", grammars.len());
    for g in &grammars {
        println!("    {}", g.file_name().unwrap().to_string_lossy());
    }

    Ok(grammars)
}
