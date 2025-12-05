use std::sync::Mutex;
use std::time::Instant;

use camino::{Utf8Path, Utf8PathBuf};
use chrono::Utc;
use miette::{Context, IntoDiagnostic, Result};
use owo_colors::OwoColorize;
use rayon::prelude::*;

use crate::tool::Tool;
use crate::types::CrateRegistry;
use crate::version_store;

pub struct BuildOptions {
    pub grammars: Vec<String>,
    pub output_dir: Option<Utf8PathBuf>,
    pub transpile: bool,
    pub profile: bool,
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            grammars: Vec::new(),
            output_dir: None,
            transpile: true,
            profile: false,
        }
    }
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginTiming {
    pub grammar: String,
    pub build_ms: u64,
    pub cargo_component_ms: u64,
    pub transpile_ms: u64,
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginTimings {
    pub recorded_at: String,
    pub timings: Vec<PluginTiming>,
}

impl PluginTimings {
    pub fn load(path: &Utf8Path) -> miette::Result<Self> {
        let content = fs_err::read_to_string(path)
            .map_err(|e| miette::miette!("failed to read {}: {}", path, e))?;
        facet_json::from_str(&content)
            .map_err(|e| miette::miette!("failed to parse {}: {}", path, e))
    }

    pub fn save(&self, path: &Utf8Path) -> miette::Result<()> {
        let content = facet_json::to_string_pretty(self);
        fs_err::write(path, content)
            .map_err(|e| miette::miette!("failed to write {}: {}", path, e))?;
        Ok(())
    }
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginGroup {
    pub index: usize,
    pub grammars: Vec<String>,
    pub total_ms: u64,
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginGroups {
    pub groups: Vec<PluginGroup>,
    pub max_group_ms: u64,
    pub ideal_per_group_ms: u64,
    pub efficiency: f64,
}

impl PluginGroups {
    pub fn from_timings(timings: &PluginTimings, num_groups: usize) -> Self {
        let num_groups = num_groups.max(1);
        let mut sorted: Vec<_> = timings.timings.iter().collect();
        sorted.sort_by(|a, b| b.build_ms.cmp(&a.build_ms));
        let mut groups: Vec<PluginGroup> = (0..num_groups)
            .map(|i| PluginGroup {
                index: i,
                grammars: Vec::new(),
                total_ms: 0,
            })
            .collect();
        for timing in sorted {
            let g = groups.iter_mut().min_by_key(|g| g.total_ms).expect("group");
            g.grammars.push(timing.grammar.clone());
            g.total_ms += timing.build_ms;
        }
        groups.retain(|g| !g.grammars.is_empty());
        for (i, g) in groups.iter_mut().enumerate() {
            g.index = i;
        }
        let max_group_ms = groups.iter().map(|g| g.total_ms).max().unwrap_or(0);
        let total_ms: u64 = timings.timings.iter().map(|t| t.build_ms).sum();
        let ideal_per_group_ms = if num_groups > 0 {
            total_ms / num_groups as u64
        } else {
            0
        };
        let efficiency = if max_group_ms > 0 {
            ideal_per_group_ms as f64 / max_group_ms as f64
        } else {
            0.0
        };
        Self {
            groups,
            max_group_ms,
            ideal_per_group_ms,
            efficiency,
        }
    }
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginManifestEntry {
    pub language: String,
    pub package: String,
    pub version: String,
    pub cdn_js: String,
    pub cdn_wasm: String,
    pub local_js: String,
    pub local_wasm: String,
}

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
pub struct PluginManifest {
    pub generated_at: String,
    pub entries: Vec<PluginManifestEntry>,
}

pub fn build_plugins(repo_root: &Utf8Path, options: &BuildOptions) -> Result<()> {
    let crates_dir = repo_root.join("crates");
    let version = version_store::read_version(repo_root)?;

    let registry = CrateRegistry::load(&crates_dir)
        .map_err(|e| miette::miette!("failed to load crate registry: {}", e))?;

    let grammars: Vec<String> = if options.grammars.is_empty() {
        registry
            .all_grammars()
            .filter(|(_, _, grammar)| grammar.generate_component())
            .map(|(_, _, grammar)| grammar.id().to_string())
            .collect()
    } else {
        options.grammars.clone()
    };

    if grammars.is_empty() {
        println!(
            "{} No grammars have generate-component enabled",
            "○".dimmed()
        );
        return Ok(());
    }

    println!(
        "{} Building {} plugin(s): {}",
        "●".cyan(),
        grammars.len(),
        grammars.join(", ")
    );

    let cargo_component = Tool::CargoComponent
        .find()
        .into_diagnostic()
        .context("cargo-component not found")?;
    let jco = if options.transpile {
        Some(
            Tool::Jco
                .find()
                .into_diagnostic()
                .context("jco not found")?,
        )
    } else {
        None
    };

    let timings: Mutex<Vec<PluginTiming>> = Mutex::new(Vec::new());
    let errors: Mutex<Vec<String>> = Mutex::new(Vec::new());

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(16)
        .build()
        .expect("failed to create thread pool");

    pool.install(|| {
        grammars.par_iter().for_each(|grammar| {
            let result = build_single_plugin(
                repo_root,
                &registry,
                grammar,
                options.output_dir.as_deref(),
                &version,
                &cargo_component,
                jco.as_ref(),
                options.profile,
            );

            match result {
                Ok(timing) => {
                    timings.lock().unwrap().push(timing);
                }
                Err(e) => {
                    errors.lock().unwrap().push(format!("{}: {}", grammar, e));
                }
            }
        })
    });

    let errors = errors.into_inner().unwrap();
    if !errors.is_empty() {
        for err in &errors {
            eprintln!("{} {}", "✗".red(), err);
        }
        miette::bail!("{} plugin(s) failed to build", errors.len());
    }

    let timings = timings.into_inner().unwrap();

    let manifest = build_manifest(
        repo_root,
        &registry,
        &grammars,
        options.output_dir.as_deref(),
        &version,
    )?;
    let manifest_path = repo_root.join("langs").join("plugins.json");
    fs_err::create_dir_all(manifest_path.parent().unwrap())
        .into_diagnostic()
        .context("failed to create manifest dir")?;
    fs_err::write(&manifest_path, facet_json::to_string_pretty(&manifest))
        .into_diagnostic()
        .context("failed to write manifest")?;
    println!(
        "{} Wrote plugin manifest {}",
        "✓".green(),
        manifest_path.cyan()
    );

    if options.profile {
        let timings_path = repo_root.join("plugin-timings.json");
        let plugin_timings = PluginTimings {
            recorded_at: Utc::now().to_rfc3339(),
            timings,
        };
        plugin_timings.save(&timings_path)?;
        println!("\n{} Saved timings to {}", "✓".green(), timings_path.cyan());
    }

    Ok(())
}

pub fn clean_plugins(repo_root: &Utf8Path, output_dir: &str) -> Result<()> {
    let output_path = repo_root.join(output_dir);
    if output_path.exists() {
        std::fs::remove_dir_all(&output_path)
            .into_diagnostic()
            .context("failed to remove output directory")?;
        println!("{} Removed {}", "✓".green(), output_path);
    } else {
        println!("{} Nothing to clean", "○".dimmed());
    }
    Ok(())
}

fn build_single_plugin(
    repo_root: &Utf8Path,
    registry: &CrateRegistry,
    grammar: &str,
    output_override: Option<&Utf8Path>,
    _version: &str,
    cargo_component: &crate::tool::ToolPath,
    jco: Option<&crate::tool::ToolPath>,
    profile: bool,
) -> Result<PluginTiming> {
    let grammar_start = Instant::now();
    println!("{} {}", "Building:".cyan(), grammar);

    let (crate_state, _) = locate_grammar(registry, grammar).ok_or_else(|| {
        miette::miette!(
            "grammar `{}` not found in registry (generate components must be enabled)",
            grammar
        )
    })?;

    let grammar_crate_path = &crate_state.crate_path;

    let plugin_output = if let Some(base) = output_override {
        let base = if base.is_absolute() {
            base.to_owned()
        } else {
            repo_root.join(base)
        };
        base.join(grammar)
    } else {
        grammar_crate_path
            .parent()
            .expect("lang directory")
            .join("npm")
    };

    // Plugin crate files (Cargo.toml, src/lib.rs, package.json) are now generated
    // by `cargo xtask gen`. Verify they exist before building.
    let cargo_toml = plugin_output.join("Cargo.toml");
    let lib_rs = plugin_output.join("src/lib.rs");
    if !cargo_toml.exists() || !lib_rs.exists() {
        miette::bail!(
            "Plugin crate files not found at {}. Run `cargo xtask gen --version <version>` first.",
            plugin_output
        );
    }

    let cargo_start = Instant::now();
    let output = cargo_component
        .command()
        .args(["build", "--release", "--target", "wasm32-wasip1"])
        .current_dir(&plugin_output)
        .output()
        .into_diagnostic()
        .context("failed to run cargo-component")?;
    let cargo_component_ms = cargo_start.elapsed().as_millis() as u64;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!("cargo-component build failed:\n{}", stderr);
    }

    let wasm_file = plugin_output
        .join("target/wasm32-wasip1/release")
        .join(format!(
            "arborium_{}_plugin.wasm",
            grammar.replace('-', "_")
        ));

    if !wasm_file.exists() {
        miette::bail!("expected wasm file not found: {}", wasm_file);
    }

    let dest_wasm = plugin_output.join("grammar.wasm");
    std::fs::copy(&wasm_file, &dest_wasm)
        .into_diagnostic()
        .context("failed to copy wasm file")?;

    let mut transpile_ms = 0u64;
    if let Some(jco) = jco {
        let transpile_start = Instant::now();
        let output = jco
            .command()
            .args([
                "transpile",
                dest_wasm.as_str(),
                "--instantiation",
                "async",
                "--quiet",
                "-o",
                plugin_output.as_str(),
            ])
            .output()
            .into_diagnostic()
            .context("failed to run jco")?;
        transpile_ms = transpile_start.elapsed().as_millis() as u64;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            miette::bail!("jco transpile failed:\n{}", stderr);
        }
    }

    let build_ms = grammar_start.elapsed().as_millis() as u64;

    if profile {
        println!(
            "    {} {}ms (cargo: {}ms, jco: {}ms)",
            "⏱".dimmed(),
            build_ms,
            cargo_component_ms,
            transpile_ms
        );
    }

    Ok(PluginTiming {
        grammar: grammar.to_string(),
        build_ms,
        cargo_component_ms,
        transpile_ms,
    })
}

fn locate_grammar<'a>(
    registry: &'a CrateRegistry,
    grammar: &str,
) -> Option<(
    &'a crate::types::CrateState,
    &'a crate::types::GrammarConfig,
)> {
    registry.configured_crates().find_map(|(_, state, cfg)| {
        cfg.grammars
            .iter()
            .find(|g| <String as AsRef<str>>::as_ref(&g.id.value) == grammar)
            .map(|g| (state, g))
    })
}

fn build_manifest(
    repo_root: &Utf8Path,
    registry: &CrateRegistry,
    grammars: &[String],
    output_override: Option<&Utf8Path>,
    version: &str,
) -> Result<PluginManifest> {
    let mut entries = Vec::new();

    for grammar in grammars {
        let (state, _) = locate_grammar(registry, grammar)
            .ok_or_else(|| miette::miette!("grammar `{}` not found for manifest", grammar))?;

        let local_root = if let Some(base) = output_override {
            if base.is_absolute() {
                base.to_owned()
            } else {
                repo_root.join(base)
            }
        } else {
            state
                .crate_path
                .parent()
                .expect("lang directory")
                .join("npm")
        };
        let local_js = local_root.join("grammar.js");
        let local_wasm = local_root.join("grammar.core.wasm");

        let package = format!("@arborium/{}", grammar);
        let cdn_base = format!(
            "https://cdn.jsdelivr.net/npm/@arborium/{}@{}",
            grammar, version
        );

        entries.push(PluginManifestEntry {
            language: grammar.clone(),
            package: package.clone(),
            version: version.to_string(),
            cdn_js: format!("{}/grammar.js", cdn_base),
            cdn_wasm: format!("{}/grammar.core.wasm", cdn_base),
            local_js: format!("/{}", local_js),
            local_wasm: format!("/{}", local_wasm),
        });
    }

    Ok(PluginManifest {
        generated_at: Utc::now().to_rfc3339(),
        entries,
    })
}
