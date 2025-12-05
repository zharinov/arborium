use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};

const VERSION_FILE: &str = "version.json";

#[derive(Debug, Clone, facet::Facet)]
#[facet(rename_all = "snake_case")]
struct VersionEntry {
    pub version: String,
}

pub fn write_version(repo_root: &Utf8Path, version: &str) -> Result<()> {
    let path = repo_root.join(VERSION_FILE);
    let entry = VersionEntry {
        version: version.to_string(),
    };
    let content = facet_json::to_string_pretty(&entry);
    fs_err::write(&path, content)
        .into_diagnostic()
        .context("failed to write version.json")?;
    Ok(())
}

pub fn read_version(repo_root: &Utf8Path) -> Result<String> {
    let path = repo_root.join(VERSION_FILE);
    let content = fs_err::read_to_string(&path)
        .into_diagnostic()
        .context("failed to read version.json; run `cargo xtask gen --version <x.y.z>`")?;
    let entry: VersionEntry = facet_json::from_str(&content)
        .into_diagnostic()
        .context("failed to parse version.json")?;
    Ok(entry.version)
}
