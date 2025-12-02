//! Linting for info.toml files and grammar highlighting

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use arborium::tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};
use arborium::HIGHLIGHT_NAMES;
use owo_colors::OwoColorize;

use crate::util::find_repo_root;

/// Required fields for info.toml
const REQUIRED_FIELDS: &[&str] = &[
    "name",
    "repo",
    "commit",
    "license",
    "description",
];

/// Recommended fields for info.toml
const RECOMMENDED_FIELDS: &[&str] = &[
    "inventor",
    "year",
    "link",
    "trivia",
    "handpicked",
];

/// Sample required fields
const SAMPLE_REQUIRED_FIELDS: &[&str] = &[
    "path",
    "description",
    "link",
    "license",
];

/// Minimum number of lines for a sample file
const MIN_SAMPLE_LINES: usize = 25;

/// Crates that don't need info.toml (internal/utility crates, sub-grammars)
const SKIP_CRATES: &[&str] = &[
    "sysroot",
    "test-harness",
    "yuri",
    // Sub-grammars that are part of a parent grammar
    "asciidoc_inline",
    "markdown-inline",
    "interface",  // OCaml interface sub-grammar
    "type",       // OCaml type sub-grammar
];

/// Result of linting a single info.toml file
#[derive(Default)]
struct LintResult {
    errors: Vec<String>,
    warnings: Vec<String>,
}

impl LintResult {
    fn has_issues(&self) -> bool {
        !self.errors.is_empty() || !self.warnings.is_empty()
    }
}

/// Lint all info.toml files in the crates directory
pub fn lint_info_toml() {
    let repo_root = find_repo_root().expect("Could not find repo root");
    let crates_dir = repo_root.join("crates");

    println!("{}", "Linting info.toml files...".cyan().bold());
    println!();

    let mut total_errors = 0;
    let mut total_warnings = 0;
    let mut crates_checked = 0;
    let mut crates_missing_info = Vec::new();

    // Find all arborium-* crates
    let mut entries: Vec<_> = fs::read_dir(&crates_dir)
        .expect("Could not read crates directory")
        .filter_map(|e| e.ok())
        .filter(|e| {
            let name = e.file_name().to_string_lossy().to_string();
            name.starts_with("arborium-") && e.path().is_dir()
        })
        .collect();

    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let crate_name = entry.file_name().to_string_lossy().to_string();
        let lang_name = crate_name.strip_prefix("arborium-").unwrap_or(&crate_name);

        // Skip internal/utility crates
        if SKIP_CRATES.contains(&lang_name) {
            continue;
        }

        let info_path = entry.path().join("info.toml");

        if !info_path.exists() {
            crates_missing_info.push(lang_name.to_string());
            continue;
        }

        crates_checked += 1;
        let result = lint_single_info_toml(&info_path, lang_name);

        if result.has_issues() {
            println!("{} {}", "●".yellow(), lang_name.bold());

            for error in &result.errors {
                println!("  {} {}", "error:".red().bold(), error);
                total_errors += 1;
            }

            for warning in &result.warnings {
                println!("  {} {}", "warning:".yellow(), warning);
                total_warnings += 1;
            }

            println!();
        }
    }

    // Summary
    println!("{}", "─".repeat(60));
    println!();

    if !crates_missing_info.is_empty() {
        println!("{} {} crate(s) missing info.toml:",
            "✗".red(),
            crates_missing_info.len());
        for name in &crates_missing_info {
            println!("  {} {}", "error:".red().bold(), format!("crates/arborium-{}/info.toml not found", name));
        }
        println!();
        total_errors += crates_missing_info.len();
    }

    println!("Checked {} crate(s)", crates_checked + crates_missing_info.len());

    if total_errors > 0 {
        println!("{} {} error(s)", "✗".red(), total_errors);
    }
    if total_warnings > 0 {
        println!("{} {} warning(s)", "⚠".yellow(), total_warnings);
    }
    if total_errors == 0 && total_warnings == 0 {
        println!("{} All info.toml files are valid!", "✓".green());
    }

    if total_errors > 0 {
        std::process::exit(1);
    }
}

/// Lint a single info.toml file
fn lint_single_info_toml(path: &Path, lang_name: &str) -> LintResult {
    let mut result = LintResult::default();

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            result.errors.push(format!("Could not read file: {}", e));
            return result;
        }
    };

    // Parse the TOML content
    let parsed: Result<toml::Value, _> = content.parse();
    let table = match parsed {
        Ok(toml::Value::Table(t)) => t,
        Ok(_) => {
            result.errors.push("info.toml root must be a table".to_string());
            return result;
        }
        Err(e) => {
            result.errors.push(format!("Invalid TOML: {}", e));
            return result;
        }
    };

    // Collect all keys for checking unknown fields
    let known_fields: HashSet<&str> = [
        "id", "name", "repo", "commit", "license", "description",
        "inventor", "year", "link", "trivia", "handpicked",
        "tag", "icon", "tier", "aliases", "samples", "subdir",
        "wikipedia", // deprecated but still recognized
    ].into_iter().collect();

    // Check for unknown top-level fields
    for key in table.keys() {
        if !known_fields.contains(key.as_str()) {
            result.warnings.push(format!("Unknown field: {}", key));
        }
    }

    // Check required fields
    for field in REQUIRED_FIELDS {
        if !table.contains_key(*field) {
            result.errors.push(format!("Missing required field: {}", field));
        } else if let Some(val) = table.get(*field) {
            if val.as_str().is_some_and(|s| s.is_empty()) {
                result.errors.push(format!("Required field '{}' is empty", field));
            }
        }
    }

    // Check recommended fields
    for field in RECOMMENDED_FIELDS {
        if !table.contains_key(*field) {
            result.warnings.push(format!("Missing recommended field: {}", field));
        } else if let Some(val) = table.get(*field) {
            if val.as_str().is_some_and(|s| s.is_empty()) {
                result.warnings.push(format!("Recommended field '{}' is empty", field));
            }
        }
    }

    // Validate field types
    if let Some(name) = table.get("name") {
        if !name.is_str() {
            result.errors.push("'name' must be a string".to_string());
        }
    }

    if let Some(year) = table.get("year") {
        match year {
            toml::Value::Integer(y) => {
                if *y < 1940 || *y > 2030 {
                    result.warnings.push(format!("'year' looks suspicious: {}", y));
                }
            }
            _ => result.errors.push("'year' must be an integer".to_string()),
        }
    }

    if let Some(repo) = table.get("repo") {
        if let Some(s) = repo.as_str() {
            if s != "local" && !s.starts_with("https://") && !s.starts_with("http://") {
                result.warnings.push(format!("'repo' should be a URL or 'local': {}", s));
            }
        } else {
            result.errors.push("'repo' must be a string".to_string());
        }
    }

    if let Some(url) = table.get("url") {
        if let Some(s) = url.as_str() {
            if !s.starts_with("https://") && !s.starts_with("http://") {
                result.errors.push(format!("'url' must be a valid URL: {}", s));
            }
        } else {
            result.errors.push("'url' must be a string".to_string());
        }
    }

    // wikipedia is deprecated - use link instead
    if table.contains_key("wikipedia") {
        result.warnings.push("'wikipedia' is deprecated, use 'link' instead".to_string());
    }

    // Validate link field
    if let Some(link) = table.get("link") {
        if let Some(s) = link.as_str() {
            if !s.starts_with("https://") && !s.starts_with("http://") {
                result.errors.push(format!("'link' must be a valid URL: {}", s));
            }
        } else {
            result.errors.push("'link' must be a string".to_string());
        }
    }

    // Validate handpicked field
    if let Some(handpicked) = table.get("handpicked") {
        if !handpicked.is_bool() {
            result.errors.push("'handpicked' must be a boolean".to_string());
        }
    }

    if let Some(aliases) = table.get("aliases") {
        if !aliases.is_array() {
            result.errors.push("'aliases' must be an array".to_string());
        } else if let Some(arr) = aliases.as_array() {
            for (i, alias) in arr.iter().enumerate() {
                if !alias.is_str() {
                    result.errors.push(format!("'aliases[{}]' must be a string", i));
                }
            }
        }
    }

    if let Some(tag) = table.get("tag") {
        if let Some(s) = tag.as_str() {
            let valid_tags = ["code", "markup", "config", "data", "shell", "query", "build"];
            if !valid_tags.contains(&s) {
                result.warnings.push(format!("'tag' should be one of: {:?}", valid_tags));
            }
        } else {
            result.errors.push("'tag' must be a string".to_string());
        }
    }

    // Check samples array
    if let Some(samples) = table.get("samples") {
        if let Some(arr) = samples.as_array() {
            if arr.is_empty() {
                result.errors.push("'samples' array is empty - at least one sample is required".to_string());
            }
            for (i, sample) in arr.iter().enumerate() {
                if let Some(sample_table) = sample.as_table() {
                    // Check sample required fields
                    for field in SAMPLE_REQUIRED_FIELDS {
                        if !sample_table.contains_key(*field) {
                            result.warnings.push(format!("samples[{}] missing field: {}", i, field));
                        }
                    }

                    // Check sample path exists and has enough lines
                    if let Some(path_val) = sample_table.get("path") {
                        if let Some(sample_path) = path_val.as_str() {
                            if sample_path.is_empty() {
                                result.errors.push(format!("samples[{}].path is empty", i));
                            } else {
                                let full_path = path.parent().unwrap().join(sample_path);
                                if !full_path.exists() {
                                    result.errors.push(format!("samples[{}].path does not exist: {}", i, sample_path));
                                } else if let Ok(content) = fs::read_to_string(&full_path) {
                                    let line_count = content.lines().count();
                                    let trimmed = content.trim();
                                    if trimmed.is_empty() {
                                        result.errors.push(format!(
                                            "samples[{}].path is empty: {}",
                                            i, sample_path
                                        ));
                                    } else if trimmed.starts_with("404:") || trimmed.starts_with("<!DOCTYPE") || trimmed == "Not Found" {
                                        result.errors.push(format!(
                                            "samples[{}].path contains HTTP error response (failed download?): {}",
                                            i, sample_path
                                        ));
                                    } else if line_count < MIN_SAMPLE_LINES {
                                        result.warnings.push(format!(
                                            "samples[{}].path has only {} lines (minimum {} recommended): {}",
                                            i, line_count, MIN_SAMPLE_LINES, sample_path
                                        ));
                                    }
                                }
                            }
                        }
                    }

                    // Check sample description is not empty
                    if let Some(desc) = sample_table.get("description") {
                        if desc.as_str().is_some_and(|s| s.is_empty()) {
                            result.errors.push(format!("samples[{}].description is empty", i));
                        }
                    }

                    // Check sample link (warn if empty, but allow "local")
                    if let Some(link) = sample_table.get("link") {
                        if let Some(s) = link.as_str() {
                            if s.is_empty() {
                                result.warnings.push(format!("samples[{}].link is empty", i));
                            } else if s != "local" && !s.starts_with("https://") && !s.starts_with("http://") {
                                result.warnings.push(format!("samples[{}].link should be a URL or 'local': {}", i, s));
                            }
                        }
                    }
                } else {
                    result.errors.push(format!("samples[{}] must be a table", i));
                }
            }
        } else {
            result.errors.push("'samples' must be an array of tables (use [[samples]])".to_string());
        }
    } else {
        // No samples section at all
        result.errors.push("Missing 'samples' section - at least one [[samples]] entry is required".to_string());
    }

    // Check that name matches directory
    if let Some(name) = table.get("name").and_then(|v| v.as_str()) {
        let expected_name = lang_name.replace('-', "_");
        let actual_name = name.to_lowercase().replace('-', "_").replace(' ', "_");
        // Allow some flexibility - just warn if very different
        if !actual_name.contains(&expected_name) && !expected_name.contains(&actual_name) {
            result.warnings.push(format!(
                "'name' ({}) doesn't match crate name (arborium-{})",
                name, lang_name
            ));
        }
    }

    result
}

/// Lint highlighting for all grammars - check that samples produce highlights
pub fn lint_highlights() {
    let repo_root = find_repo_root().expect("Could not find repo root");
    let crates_dir = repo_root.join("crates");

    println!("{}", "Checking highlighting for all grammars...".cyan().bold());
    println!();

    let mut total_errors = 0;
    let mut total_warnings = 0;
    let mut crates_checked = 0;

    // Find all arborium-* crates
    let mut entries: Vec<_> = fs::read_dir(&crates_dir)
        .expect("Could not read crates directory")
        .filter_map(|e| e.ok())
        .filter(|e| {
            let name = e.file_name().to_string_lossy().to_string();
            name.starts_with("arborium-") && e.path().is_dir()
        })
        .collect();

    entries.sort_by_key(|e| e.file_name());

    let names: Vec<String> = HIGHLIGHT_NAMES.iter().map(|s| s.to_string()).collect();

    for entry in entries {
        let crate_name = entry.file_name().to_string_lossy().to_string();
        let lang_name = crate_name.strip_prefix("arborium-").unwrap_or(&crate_name);

        // Skip internal/utility crates
        if SKIP_CRATES.contains(&lang_name) {
            continue;
        }

        let crate_path = entry.path();

        // Check if we have queries
        let queries_dir = crate_path.join("queries");
        let highlights_scm = queries_dir.join("highlights.scm");

        if !highlights_scm.exists() {
            println!("{} {} - {}", "✗".red(), lang_name.bold(), "no highlights.scm".red());
            total_errors += 1;
            continue;
        }

        // Try to find a sample file
        let samples_dir = crate_path.join("samples");
        let sample_file = if samples_dir.exists() {
            fs::read_dir(&samples_dir)
                .ok()
                .and_then(|mut entries| entries.next())
                .and_then(|e| e.ok())
                .map(|e| e.path())
        } else {
            None
        };

        let sample_content = sample_file
            .as_ref()
            .and_then(|p| fs::read_to_string(p).ok());

        if sample_content.is_none() {
            println!("{} {} - {}", "⚠".yellow(), lang_name.bold(), "no sample file".yellow());
            total_warnings += 1;
            continue;
        }

        let sample_content = sample_content.unwrap();

        // Try to create a highlight configuration and highlight the sample
        let result = check_highlighting(lang_name, &crate_path, &sample_content, &names);

        crates_checked += 1;

        match result {
            HighlightCheckResult::Ok { highlight_count } => {
                if highlight_count == 0 {
                    println!("{} {} - {}", "✗".red(), lang_name.bold(), "0 highlights produced".red());
                    total_errors += 1;
                } else {
                    println!("{} {} - {} highlights", "✓".green(), lang_name, highlight_count);
                }
            }
            HighlightCheckResult::QueryError(e) => {
                println!("{} {} - query error: {}", "✗".red(), lang_name.bold(), e.red());
                total_errors += 1;
            }
            HighlightCheckResult::HighlightError(e) => {
                println!("{} {} - highlight error: {}", "✗".red(), lang_name.bold(), e.red());
                total_errors += 1;
            }
            HighlightCheckResult::NoLanguageFn => {
                println!("{} {} - {}", "⚠".yellow(), lang_name.bold(), "could not load language".yellow());
                total_warnings += 1;
            }
        }
    }

    // Summary
    println!();
    println!("{}", "─".repeat(60));
    println!();
    println!("Checked {} grammar(s)", crates_checked);

    if total_errors > 0 {
        println!("{} {} error(s)", "✗".red(), total_errors);
    }
    if total_warnings > 0 {
        println!("{} {} warning(s)", "⚠".yellow(), total_warnings);
    }
    if total_errors == 0 && total_warnings == 0 {
        println!("{} All grammars produce highlights!", "✓".green());
    }

    if total_errors > 0 {
        std::process::exit(1);
    }
}

enum HighlightCheckResult {
    Ok { highlight_count: usize },
    QueryError(String),
    HighlightError(String),
    NoLanguageFn,
}

/// Check highlighting for a single grammar
fn check_highlighting(
    lang_name: &str,
    crate_path: &Path,
    sample: &str,
    names: &[String],
) -> HighlightCheckResult {
    // Load queries
    let queries_dir = crate_path.join("queries");
    let highlights = fs::read_to_string(queries_dir.join("highlights.scm")).unwrap_or_default();
    let injections = fs::read_to_string(queries_dir.join("injections.scm")).unwrap_or_default();
    let locals = fs::read_to_string(queries_dir.join("locals.scm")).unwrap_or_default();

    // Get language - we need to dynamically load it based on lang_name
    let language = match get_language_for_name(lang_name) {
        Some(l) => l,
        None => return HighlightCheckResult::NoLanguageFn,
    };

    // Create highlight configuration
    let config = HighlightConfiguration::new(
        language.into(),
        lang_name,
        &highlights,
        &injections,
        &locals,
    );

    let mut config = match config {
        Ok(c) => c,
        Err(e) => return HighlightCheckResult::QueryError(format!("{:?}", e)),
    };

    config.configure(names);

    // Run highlighter
    let mut highlighter = Highlighter::new();
    let highlights_iter = highlighter.highlight(&config, sample.as_bytes(), None, |_| None);

    let highlights_iter = match highlights_iter {
        Ok(h) => h,
        Err(e) => return HighlightCheckResult::HighlightError(format!("{:?}", e)),
    };

    // Count highlights
    let mut highlight_count = 0;
    for event in highlights_iter {
        match event {
            Ok(HighlightEvent::HighlightStart(_)) => {
                highlight_count += 1;
            }
            Ok(_) => {}
            Err(e) => return HighlightCheckResult::HighlightError(format!("{:?}", e)),
        }
    }

    HighlightCheckResult::Ok { highlight_count }
}

/// Get the tree-sitter Language for a given language name
fn get_language_for_name(name: &str) -> Option<arborium::tree_sitter::Language> {
    // This macro generates the match arms for all enabled languages
    macro_rules! lang_match {
        ($($lang:ident => $mod:ident),* $(,)?) => {
            match name {
                $(
                    stringify!($lang) => Some(arborium::$mod::language()),
                )*
                _ => None,
            }
        };
    }

    // Handle hyphenated names specially
    match name {
        "c-sharp" => return Some(arborium::lang_c_sharp::language()),
        "ssh-config" => return Some(arborium::lang_ssh_config::language()),
        _ => {}
    }

    lang_match! {
        ada => lang_ada,
        agda => lang_agda,
        asm => lang_asm,
        awk => lang_awk,
        bash => lang_bash,
        batch => lang_batch,
        c => lang_c,
        caddy => lang_caddy,
        capnp => lang_capnp,
        clojure => lang_clojure,
        cmake => lang_cmake,
        commonlisp => lang_commonlisp,
        cpp => lang_cpp,
        css => lang_css,
        d => lang_d,
        dart => lang_dart,
        devicetree => lang_devicetree,
        diff => lang_diff,
        dockerfile => lang_dockerfile,
        dot => lang_dot,
        elisp => lang_elisp,
        elixir => lang_elixir,
        elm => lang_elm,
        erlang => lang_erlang,
        fish => lang_fish,
        fsharp => lang_fsharp,
        gleam => lang_gleam,
        glsl => lang_glsl,
        go => lang_go,
        graphql => lang_graphql,
        haskell => lang_haskell,
        hcl => lang_hcl,
        hlsl => lang_hlsl,
        html => lang_html,
        ini => lang_ini,
        java => lang_java,
        javascript => lang_javascript,
        jinja2 => lang_jinja2,
        jq => lang_jq,
        json => lang_json,
        julia => lang_julia,
        kdl => lang_kdl,
        kotlin => lang_kotlin,
        lean => lang_lean,
        lua => lang_lua,
        matlab => lang_matlab,
        meson => lang_meson,
        nginx => lang_nginx,
        ninja => lang_ninja,
        nix => lang_nix,
        objc => lang_objc,
        ocaml => lang_ocaml,
        perl => lang_perl,
        php => lang_php,
        powershell => lang_powershell,
        prolog => lang_prolog,
        python => lang_python,
        query => lang_query,
        r => lang_r,
        rescript => lang_rescript,
        ron => lang_ron,
        ruby => lang_ruby,
        rust => lang_rust,
        scala => lang_scala,
        scheme => lang_scheme,
        scss => lang_scss,
        sparql => lang_sparql,
        sql => lang_sql,
        starlark => lang_starlark,
        svelte => lang_svelte,
        swift => lang_swift,
        textproto => lang_textproto,
        thrift => lang_thrift,
        tlaplus => lang_tlaplus,
        toml => lang_toml,
        tsx => lang_tsx,
        typescript => lang_typescript,
        typst => lang_typst,
        uiua => lang_uiua,
        vb => lang_vb,
        verilog => lang_verilog,
        vhdl => lang_vhdl,
        vue => lang_vue,
        x86asm => lang_x86asm,
        xml => lang_xml,
        yaml => lang_yaml,
        yuri => lang_yuri,
        zig => lang_zig,
    }
}
