// =============================================================================
// Grammar crate generation
// =============================================================================

/// Configuration for a grammar crate
///
/// This can be auto-detected from the filesystem, or loaded from a
/// `grammar-crate-config.toml` file in the grammar directory for special cases.
#[derive(Debug)]
struct GrammarCrateConfig {
    /// Grammar name (e.g., "rust", "python")
    name: String,
    /// C function name suffix (e.g., "rust" for tree_sitter_rust)
    c_symbol: String,
    /// Source files to compile (relative to src/)
    source_files: Vec<String>,
    /// Whether highlights.scm exists
    has_highlights: bool,
    /// Whether injections.scm exists
    has_injections: bool,
    /// Whether locals.scm exists
    has_locals: bool,
    /// Query path prefix (for grammars with nested query directories)
    query_path: String,
    /// Additional languages exported by this grammar (e.g., "tsx" for typescript)
    extra_languages: Vec<(String, String)>, // (c_symbol, export_name)
    /// Sample files for testing (paths relative to crate root)
    samples: Vec<String>,
    /// For sub-grammars: the parent repo name (e.g., "typescript" for tsx in tree-sitter-typescript)
    /// Used to find queries in grammars/tree-sitter-{parent_repo}/queries/ instead of tree-sitter-{name}
    parent_repo: Option<String>,
    /// Base languages whose queries should be included before this grammar's queries
    /// e.g., ["javascript"] for TypeScript means JavaScript queries are prepended
    inherits_queries_from: Vec<String>,
}

/// Generate all grammar crates
fn generate_grammar_crates() {
    let repo_root = find_repo_root().expect("Could not find repo root");
    println!("Generating grammar crates...\n");

    // Parse GRAMMARS.toml to get license info
    let grammars = match parse_grammars_toml(&repo_root) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Error parsing GRAMMARS.toml: {}", e);
            std::process::exit(1);
        }
    };

    // Find all grammar directories
    let grammar_dirs = find_grammars(&repo_root).expect("Failed to find grammars");

    let crates_dir = repo_root.join("crates");
    if !crates_dir.exists() {
        fs::create_dir_all(&crates_dir).expect("Failed to create crates/");
    }

    let mut generated = 0;
    let mut skipped = 0;
    let mut workspace_members = Vec::new();

    for grammar_dir in grammar_dirs {
        let dir_name = grammar_dir.file_name().unwrap().to_string_lossy();
        let name = dir_name.strip_prefix("tree-sitter-").unwrap_or(&dir_name);

        // Skip grammars we don't have in GRAMMARS.toml
        let license = grammars
            .get(name)
            .map(|g| g.license.as_str())
            .unwrap_or("MIT");

        // Detect configuration
        let config = detect_grammar_config(&repo_root, &grammar_dir, name);

        // Create crate directory
        let crate_name = format!("arborium-{}", name);
        let crate_dir = crates_dir.join(&crate_name);
        let src_dir = crate_dir.join("src");

        // Skip if crate already exists (preserve manual edits)
        if crate_dir.exists() {
            println!("  {} (skipped - already exists)", crate_name);
            skipped += 1;
            workspace_members.push(format!("crates/{}", crate_name));
            continue;
        }

        println!("  Generating {}...", crate_name);

        fs::create_dir_all(&src_dir).expect("Failed to create src/");

        // Generate files
        let cargo_toml = generate_cargo_toml(&config, license);
        let build_rs = generate_build_rs(&config);
        let lib_rs = generate_lib_rs(&config);

        fs::write(crate_dir.join("Cargo.toml"), cargo_toml).expect("Failed to write Cargo.toml");
        fs::write(crate_dir.join("build.rs"), build_rs).expect("Failed to write build.rs");
        fs::write(src_dir.join("lib.rs"), lib_rs).expect("Failed to write lib.rs");

        workspace_members.push(format!("crates/{}", crate_name));
        generated += 1;
    }

    println!();
    println!(
        "Generated {} crates, skipped {} existing",
        generated, skipped
    );
    println!();

    // Print workspace members for Cargo.toml
    println!("Add these to workspace.members in Cargo.toml:");
    println!();
    for member in &workspace_members {
        println!("    \"{}\",", member);
    }
}

/// Update existing grammar crates to use the test harness
fn update_grammar_crates() {
    let repo_root = find_repo_root().expect("Could not find repo root");
    println!("Updating grammar crates to use test harness...\n");

    // Parse GRAMMARS.toml to get license info
    let grammars = match parse_grammars_toml(&repo_root) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Error parsing GRAMMARS.toml: {}", e);
            std::process::exit(1);
        }
    };

    // Find all grammar directories
    let grammar_dirs = find_grammars(&repo_root).expect("Failed to find grammars");

    let crates_dir = repo_root.join("crates");
    let mut updated = 0;
    let mut skipped = 0;

    for grammar_dir in grammar_dirs {
        let dir_name = grammar_dir.file_name().unwrap().to_string_lossy();
        let name = dir_name.strip_prefix("tree-sitter-").unwrap_or(&dir_name);

        let crate_name = format!("arborium-{}", name);
        let crate_dir = crates_dir.join(&crate_name);

        // Skip if crate doesn't exist
        if !crate_dir.exists() {
            skipped += 1;
            continue;
        }

        let license = grammars
            .get(name)
            .map(|g| g.license.as_str())
            .unwrap_or("MIT");
        let config = detect_grammar_config(&repo_root, &grammar_dir, name);

        println!("  Updating {}...", crate_name);

        // Regenerate lib.rs with new test harness
        let lib_rs = generate_lib_rs(&config);
        let src_dir = crate_dir.join("src");
        fs::write(src_dir.join("lib.rs"), lib_rs).expect("Failed to write lib.rs");

        // Update Cargo.toml to add dev-dependency if not present
        let cargo_toml_path = crate_dir.join("Cargo.toml");
        let cargo_content =
            fs::read_to_string(&cargo_toml_path).expect("Failed to read Cargo.toml");

        if !cargo_content.contains("arborium-test-harness") {
            // Regenerate entire Cargo.toml with dev-dependency
            let cargo_toml = generate_cargo_toml(&config, license);
            fs::write(&cargo_toml_path, cargo_toml).expect("Failed to write Cargo.toml");
        }

        updated += 1;
    }

    println!();
    println!(
        "Updated {} crates, skipped {} non-existent",
        updated, skipped
    );
}

/// Generate lib.rs for a grammar crate
fn generate_lib_rs(config: &GrammarCrateConfig) -> String {
    let mut output = String::new();

    // Module doc comment
    output.push_str(&format!(
        r#"//! {} grammar for tree-sitter
//!
//! This crate provides the {} language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

"#,
        config.name.to_uppercase(),
        config.name,
    ));

    // Main language extern
    output.push_str(&format!(
        r#"unsafe extern "C" {{
    fn tree_sitter_{}() -> Language;
}}

/// Returns the {} tree-sitter language.
pub fn language() -> Language {{
    unsafe {{ tree_sitter_{}() }}
}}

"#,
        config.c_symbol, config.name, config.c_symbol,
    ));

    // Extra languages
    for (c_symbol, export_name) in &config.extra_languages {
        output.push_str(&format!(
            r#"unsafe extern "C" {{
    fn tree_sitter_{}() -> Language;
}}

/// Returns the {} tree-sitter language.
pub fn {}_language() -> Language {{
    unsafe {{ tree_sitter_{}() }}
}}

"#,
            c_symbol, export_name, export_name, c_symbol,
        ));
    }

    // Query constants - queries are vendored in the crate's queries/ directory
    // For grammars with inherits_queries_from, we concatenate inherited queries first

    // Helper to generate query constant with optional inheritance
    let generate_query_const = |query_type: &str, has_query: bool| -> String {
        let query_file = format!("{}.scm", query_type);
        let const_name = format!("{}_QUERY", query_type.to_uppercase());

        if !has_query && config.inherits_queries_from.is_empty() {
            return format!(
                r#"/// The {} query for {} (empty - no {} available).
pub const {}: &str = "";

"#,
                query_type, config.name, query_type, const_name
            );
        }

        // Build the include_str! parts
        let mut parts: Vec<String> = Vec::new();

        // Add inherited queries first (they get prepended)
        for base_lang in &config.inherits_queries_from {
            let inherited_file = format!("inherited-{}-{}", base_lang, query_file);
            parts.push(format!(r#"include_str!("../queries/{}")"#, inherited_file));
        }

        // Add own query if it exists
        if has_query {
            parts.push(format!(r#"include_str!("../queries/{}")"#, query_file));
        }

        if parts.is_empty() {
            return format!(
                r#"/// The {} query for {} (empty - no {} available).
pub const {}: &str = "";

"#,
                query_type, config.name, query_type, const_name
            );
        }

        if parts.len() == 1 {
            format!(
                r#"/// The {} query for {}.
pub const {}: &str = {};

"#,
                query_type, config.name, const_name, parts[0]
            )
        } else {
            // Use concat! for multiple parts
            let concat_parts = parts.join(",\n    \"\\n\",\n    ");
            format!(
                r#"/// The {} query for {}.
pub const {}: &str = concat!(
    {},
);

"#,
                query_type, config.name, const_name, concat_parts
            )
        }
    };

    output.push_str(&generate_query_const("highlights", config.has_highlights));
    output.push_str(&generate_query_const("injections", config.has_injections));
    output.push_str(&generate_query_const("locals", config.has_locals));

    // Tests
    output.push_str(&format!(
        r#"#[cfg(test)]
mod tests {{
    use super::*;

    #[test]
    fn test_grammar() {{
        arborium_test_harness::test_grammar(
            language(),
            "{}",
            HIGHLIGHTS_QUERY,
            INJECTIONS_QUERY,
            LOCALS_QUERY,
            env!("CARGO_MANIFEST_DIR"),
        );
    }}
}}
"#,
        config.name,
    ));

    output
}

/// Generate build.rs for a grammar crate
fn generate_build_rs(config: &GrammarCrateConfig) -> String {
    // Source files are now in grammar-src/ within the crate itself
    let src_dir_path = "grammar-src";

    let rerun_lines: String = config
        .source_files
        .iter()
        .map(|f| {
            format!(
                "    println!(\"cargo:rerun-if-changed={{}}/{}\", src_dir);",
                f
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let file_lines: String = config
        .source_files
        .iter()
        .map(|f| format!("    build.file(format!(\"{{}}/{}\", src_dir));", f))
        .collect::<Vec<_>>()
        .join("\n");

    let compile_name = format!("tree_sitter_{}", config.c_symbol);

    format!(
        r#"fn main() {{
    let src_dir = "{src_dir}";

{rerun_lines}

    let mut build = cc::Build::new();

    build
        .include(src_dir)
        .include(format!("{{}}/tree_sitter", src_dir))
        .warnings(false)
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");

    // For WASM builds, use our custom sysroot (provided by arborium crate via links = "arborium")
    let target = std::env::var("TARGET").unwrap_or_default();
    if target.contains("wasm") {{
        if let Ok(sysroot) = std::env::var("DEP_ARBORIUM_SYSROOT_PATH") {{
            build.include(&sysroot);
        }}
    }}

{file_lines}

    build.compile("{compile_name}");
}}
"#,
        src_dir = src_dir_path,
        rerun_lines = rerun_lines,
        file_lines = file_lines,
        compile_name = compile_name,
    )
}

/// Generate Cargo.toml for a grammar crate
fn generate_cargo_toml(config: &GrammarCrateConfig, license: &str) -> String {
    let crate_name = format!("arborium-{}", config.name);
    let description = format!(
        "{} grammar for arborium (tree-sitter bindings)",
        config.name
    );

    format!(
        r#"[package]
name = "{crate_name}"
version = "0.1.0"
edition = "2024"
description = "{description}"
license = "{license}"
repository = "https://github.com/bearcove/arborium"
keywords = ["tree-sitter", "{name}", "syntax-highlighting"]
categories = ["parsing", "text-processing"]

[lib]
path = "src/lib.rs"

[dependencies]
tree-sitter-patched-arborium = {{ version = "0.25.10", path = "../../tree-sitter" }}
arborium-sysroot = {{ version = "0.1.0", path = "../arborium-sysroot" }}

[dev-dependencies]
arborium-test-harness = {{ version = "0.1.0", path = "../arborium-test-harness" }}

[build-dependencies]
cc = {{ version = "1", features = ["parallel"] }}
"#,
        crate_name = crate_name,
        description = description,
        license = license,
        name = config.name,
    )
}

/// Parse [[samples]] entries from info.toml
/// FIXME: use a TOML parser omg
fn parse_samples_from_info_toml(path: &Path) -> Vec<String> {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return vec![],
    };

    let mut samples = Vec::new();
    let mut in_samples_block = false;

    for line in content.lines() {
        let line = line.trim();

        if line == "[[samples]]" {
            in_samples_block = true;
            continue;
        }

        if in_samples_block && line.starts_with("path") {
            if let Some(value) = line.split('=').nth(1) {
                let value = value.split('#').next().unwrap_or(value);
                let value = value.trim().trim_matches('"').trim_matches('\'');
                if !value.is_empty() {
                    samples.push(value.to_string());
                }
            }
            in_samples_block = false;
        }
    }

    samples
}

/// Detect grammar configuration from filesystem and optional config file
///
/// Looks for grammar-crate-config.toml in the grammar directory for overrides.
/// Falls back to auto-detection for any unspecified values.
fn detect_grammar_config(repo_root: &Path, grammar_dir: &Path, name: &str) -> GrammarCrateConfig {
    let src_dir = grammar_dir.join("src");

    // For sub-grammars, check parent directory for queries
    let queries_dir = if grammar_dir.join("queries").exists() {
        grammar_dir.join("queries")
    } else if let Some(parent) = grammar_dir.parent() {
        if parent.join("queries").exists() {
            parent.join("queries")
        } else {
            grammar_dir.join("queries") // will be detected as not existing
        }
    } else {
        grammar_dir.join("queries")
    };

    // Load config file if it exists
    let config_file = grammar_dir.join("grammar-crate-config.toml");
    eprintln!(
        "  [DEBUG] Looking for config at: {} (exists: {})",
        config_file.display(),
        config_file.exists()
    );
    let config = parse_grammar_crate_config(&config_file);

    // Extract values from config or use defaults
    let c_symbol = config
        .as_ref()
        .and_then(|c| c.get("c_symbol"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| name.replace('-', "_"));

    let query_path = config
        .as_ref()
        .and_then(|c| c.get("query_path"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_default();

    let parent_repo = config
        .as_ref()
        .and_then(|c| c.get("parent_repo"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let inherits_queries_from = config
        .as_ref()
        .and_then(|c| c.get("inherits_queries_from"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    let extra_languages = config
        .as_ref()
        .and_then(|c| c.get("extra_languages"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| {
                    let c_sym = v.get("c_symbol")?.as_str()?;
                    let export = v.get("export_name")?.as_str()?;
                    Some((c_sym.to_string(), export.to_string()))
                })
                .collect()
        })
        .unwrap_or_default();

    // Detect source files
    let mut source_files = vec!["parser.c".to_string()];
    if src_dir.join("scanner.c").exists() {
        source_files.push("scanner.c".to_string());
    }
    if src_dir.join("scanner.cc").exists() {
        source_files.push("scanner.cc".to_string());
    }

    // Detect queries - apply query_path prefix
    let query_base = if query_path.is_empty() {
        queries_dir.clone()
    } else {
        queries_dir.join(&query_path)
    };
    let has_highlights = query_base.join("highlights.scm").exists();
    let has_injections = query_base.join("injections.scm").exists();
    let has_locals = query_base.join("locals.scm").exists();

    // Read samples from info.toml if it exists
    let crate_dir = repo_root.join("crates").join(format!("arborium-{}", name));
    let info_toml = crate_dir.join("info.toml");
    let samples = if info_toml.exists() {
        parse_samples_from_info_toml(&info_toml)
    } else {
        vec![]
    };

    GrammarCrateConfig {
        name: name.to_string(),
        c_symbol,
        source_files,
        has_highlights,
        has_injections,
        has_locals,
        query_path,
        extra_languages,
        samples,
        parent_repo,
        inherits_queries_from,
    }
}

/// Parse grammar-crate-config.toml from a grammar directory
///
/// Example grammar-crate-config.toml:
/// ```toml
/// # Optional: C symbol name (defaults to name with - replaced by _)
/// c_symbol = "rust_orchard"
///
/// # Optional: Query path prefix for nested query directories
/// query_path = "just/"
///
/// # Optional: For sub-grammars, the parent repo name for finding queries
/// parent_repo = "typescript"
///
/// # Optional: Languages whose queries should be inherited (prepended)
/// inherits_queries_from = ["javascript"]
///
/// # Optional: Additional languages exported by this grammar
/// [[extra_languages]]
/// c_symbol = "tsx"
/// export_name = "tsx"
/// ```
fn parse_grammar_crate_config(config_path: &Path) -> Option<toml::Value> {
    if config_path.exists() {
        eprintln!("  [DEBUG] Found config: {}", config_path.display());
    }
    let content = fs::read_to_string(config_path).ok()?;
    let parsed = content.parse::<toml::Value>().ok();
    if parsed.is_some() {
        eprintln!(
            "  [DEBUG] Parsed config with parent_repo: {:?}",
            parsed.as_ref().and_then(|v| v.get("parent_repo"))
        );
    }
    parsed
}

/// Parse [[samples]] entries from info.toml
/// FIXME: use a TOML parser omg
fn parse_samples_from_info_toml(path: &Path) -> Vec<String> {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return vec![],
    };

    let mut samples = Vec::new();
    let mut in_samples_block = false;

    for line in content.lines() {
        let line = line.trim();

        if line == "[[samples]]" {
            in_samples_block = true;
            continue;
        }

        if in_samples_block && line.starts_with("path") {
            if let Some(value) = line.split('=').nth(1) {
                let value = value.split('#').next().unwrap_or(value);
                let value = value.trim().trim_matches('"').trim_matches('\'');
                if !value.is_empty() {
                    samples.push(value.to_string());
                }
            }
            in_samples_block = false;
        }
    }

    samples
}

/// Copy query files from a grammar's queries/ to the corresponding crate's queries/
/// Also handles inherits_queries_from by copying base language queries
fn copy_queries_to_crate(
    repo_root: &Path,
    grammar_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let name = grammar_dir
        .file_name()
        .unwrap()
        .to_string_lossy()
        .strip_prefix("tree-sitter-")
        .unwrap_or(&grammar_dir.file_name().unwrap().to_string_lossy())
        .to_string();

    let crate_name = format!("arborium-{}", name);
    let crate_dir = repo_root.join("crates").join(&crate_name);
    let crate_queries_dir = crate_dir.join("queries");

    // Skip if crate doesn't exist
    if !crate_dir.exists() {
        return Ok(());
    }

    // Load grammar config to check for inherits_queries_from
    let config_file = grammar_dir.join("grammar-crate-config.toml");
    let config = parse_grammar_crate_config(&config_file);

    let inherits_queries_from: Vec<String> = config
        .as_ref()
        .and_then(|c| c.get("inherits_queries_from"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    let query_path = config
        .as_ref()
        .and_then(|c| c.get("query_path"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_default();

    let parent_repo = config
        .as_ref()
        .and_then(|c| c.get("parent_repo"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    // Find queries directory (may be in parent for sub-grammars)
    let queries_dir = if grammar_dir.join("queries").exists() {
        grammar_dir.join("queries")
    } else if let Some(parent) = grammar_dir.parent() {
        if parent.join("queries").exists() {
            parent.join("queries")
        } else {
            grammar_dir.join("queries")
        }
    } else {
        grammar_dir.join("queries")
    };

    // Apply query_path prefix if specified
    let query_base = if query_path.is_empty() {
        queries_dir.clone()
    } else {
        queries_dir.join(&query_path)
    };

    // Check if we have any queries to copy
    let has_own_queries = query_base.join("highlights.scm").exists()
        || query_base.join("injections.scm").exists()
        || query_base.join("locals.scm").exists();

    if !has_own_queries && inherits_queries_from.is_empty() {
        return Ok(());
    }

    // Create queries/ directory in crate
    if crate_queries_dir.exists() {
        fs::remove_dir_all(&crate_queries_dir)?;
    }
    fs::create_dir_all(&crate_queries_dir)?;

    // Copy inherited queries first (they get prepended)
    for base_lang in &inherits_queries_from {
        let base_grammar_dir = repo_root
            .join("grammars")
            .join(format!("tree-sitter-{}", base_lang));
        let base_queries_dir = base_grammar_dir.join("queries");

        for query_file in &["highlights.scm", "injections.scm", "locals.scm"] {
            let src_path = base_queries_dir.join(query_file);
            if src_path.exists() {
                // Name it as inherited-{base}-{query_file}
                let dest_name = format!("inherited-{}-{}", base_lang, query_file);
                let dest_path = crate_queries_dir.join(&dest_name);
                fs::copy(&src_path, &dest_path)?;
            }
        }
    }

    // Get extra_injections from config
    let extra_injections = config
        .as_ref()
        .and_then(|c| c.get("extra_injections"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_default();

    // Generated file header
    let header = "; DO NOT EDIT - This file is generated by `cargo xtask gen-crates`\n\
                  ; To modify, edit the source in grammars/tree-sitter-*/queries/\n\
                  ; or add extra_injections in grammars/tree-sitter-*/grammar-crate-config.toml\n\n";

    // Copy this grammar's own queries with generated header
    for query_file in &["highlights.scm", "injections.scm", "locals.scm"] {
        let src_path = query_base.join(query_file);
        if src_path.exists() {
            let dest_path = crate_queries_dir.join(query_file);
            let content = fs::read_to_string(&src_path)?;
            // Remove any "; inherits:" lines since we handle inheritance differently
            let content: String = content
                .lines()
                .filter(|line| !line.trim().starts_with("; inherits"))
                .collect::<Vec<_>>()
                .join("\n");

            // Add extra injections if this is injections.scm and we have them
            let content = if *query_file == "injections.scm" && !extra_injections.is_empty() {
                format!(
                    "{}\n\n; Extra injections from grammar-crate-config.toml:\n{}",
                    content, extra_injections
                )
            } else {
                content
            };

            fs::write(&dest_path, format!("{}{}", header, content))?;
        }
    }

    // For sub-grammars, also check parent_repo for queries if they weren't found
    if let Some(parent) = &parent_repo {
        let parent_grammar_dir = repo_root
            .join("grammars")
            .join(format!("tree-sitter-{}", parent));
        let parent_queries_base = if query_path.is_empty() {
            parent_grammar_dir.join("queries")
        } else {
            parent_grammar_dir.join("queries").join(&query_path)
        };

        for query_file in &["highlights.scm", "injections.scm", "locals.scm"] {
            let dest_path = crate_queries_dir.join(query_file);
            // Only copy if we don't already have it
            if !dest_path.exists() {
                let src_path = parent_queries_base.join(query_file);
                if src_path.exists() {
                    let content = fs::read_to_string(&src_path)?;
                    // Remove any "; inherits:" lines
                    let content: String = content
                        .lines()
                        .filter(|line| !line.trim().starts_with("; inherits"))
                        .collect::<Vec<_>>()
                        .join("\n");

                    // Add extra injections if this is injections.scm and we have them
                    let content = if *query_file == "injections.scm" && !extra_injections.is_empty()
                    {
                        format!(
                            "{}\n\n; Extra injections from grammar-crate-config.toml:\n{}",
                            content, extra_injections
                        )
                    } else {
                        content
                    };

                    fs::write(&dest_path, format!("{}{}", header, content))?;
                }
            }
        }
    }

    Ok(())
}

/// Copy C source files from a grammar's src/ to the corresponding crate's grammar-src/
fn copy_grammar_sources_to_crate(
    repo_root: &Path,
    grammar_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let name = grammar_dir
        .file_name()
        .unwrap()
        .to_string_lossy()
        .strip_prefix("tree-sitter-")
        .unwrap_or(&grammar_dir.file_name().unwrap().to_string_lossy())
        .to_string();

    let grammar_src_dir = grammar_dir.join("src");
    let crate_name = format!("arborium-{}", name);
    let crate_dir = repo_root.join("crates").join(&crate_name);
    let crate_grammar_src = crate_dir.join("grammar-src");

    // Skip if crate doesn't exist
    if !crate_dir.exists() {
        return Ok(());
    }

    // Skip if grammar src doesn't exist
    if !grammar_src_dir.exists() {
        return Ok(());
    }

    // Create grammar-src/ directory in crate
    if crate_grammar_src.exists() {
        fs::remove_dir_all(&crate_grammar_src)?;
    }
    fs::create_dir_all(&crate_grammar_src)?;

    // Copy shared directories (common/, etc.) from grammar root if they exist
    // These are used by scanners that include files like "../../common/scanner.h"
    // For sub-grammars (e.g., typescript in tree-sitter-typescript/typescript/),
    // also check the parent directory for shared files
    // For deeply nested grammars (e.g., tree-sitter-ocaml/grammars/ocaml/),
    // also check the grandparent directory
    for shared_dir in &["common"] {
        // First check in grammar_dir itself
        let src_shared = grammar_dir.join(shared_dir);
        if src_shared.exists() && src_shared.is_dir() {
            let dest_shared = crate_grammar_src.join(shared_dir);
            copy_dir_recursive(&src_shared, &dest_shared)?;
        } else if let Some(parent) = grammar_dir.parent() {
            // For sub-grammars, check the parent directory (e.g., tree-sitter-typescript/)
            let parent_shared = parent.join(shared_dir);
            if parent_shared.exists() && parent_shared.is_dir() {
                let dest_shared = crate_grammar_src.join(shared_dir);
                copy_dir_recursive(&parent_shared, &dest_shared)?;
            } else if let Some(grandparent) = parent.parent() {
                // For deeply nested grammars (e.g., tree-sitter-ocaml/grammars/ocaml/)
                let grandparent_shared = grandparent.join(shared_dir);
                if grandparent_shared.exists() && grandparent_shared.is_dir() {
                    let dest_shared = crate_grammar_src.join(shared_dir);
                    copy_dir_recursive(&grandparent_shared, &dest_shared)?;
                }
            }
        }
    }

    // Copy all .c, .cc, .h, .json files from grammar's src/
    for entry in fs::read_dir(&grammar_src_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() {
            let file_name = path.file_name().unwrap().to_string_lossy();
            let should_copy = file_name.ends_with(".c")
                || file_name.ends_with(".cc")
                || file_name.ends_with(".h")
                || file_name.ends_with(".json");

            if should_copy {
                let dest_path = crate_grammar_src.join(entry.file_name());

                // For C/C++ source files, fix include paths that reference parent directories
                // e.g., "../../common/scanner.h" -> "common/scanner.h"
                if file_name.ends_with(".c") || file_name.ends_with(".cc") {
                    let content = fs::read_to_string(&path)?;
                    let fixed_content = content
                        .replace("#include \"../../../common/", "#include \"common/")
                        .replace("#include \"../../common/", "#include \"common/")
                        .replace("#include \"../common/", "#include \"common/");
                    fs::write(&dest_path, fixed_content)?;
                } else {
                    fs::copy(&path, &dest_path)?;
                }
            }
        } else if path.is_dir() {
            // Copy subdirectories like tree_sitter/
            let dest_dir = crate_grammar_src.join(entry.file_name());
            copy_dir_recursive(&path, &dest_dir)?;
        }
    }

    Ok(())
}
