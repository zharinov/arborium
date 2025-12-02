/// Check for env imports in WASM files (these won't work in browsers)
/// This checks the cargo-built wasm BEFORE wasm-opt runs (which is slow)
fn check_wasm_env_imports_debug() -> Result<(), Box<dyn std::error::Error>> {
    let wasm_file = Path::new("target/wasm32-unknown-unknown/debug/arborium_demo.wasm");
    check_wasm_env_imports(wasm_file)
}

fn check_wasm_env_imports(wasm_file: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if !wasm_file.exists() {
        return Err(format!("WASM file not found: {}", wasm_file.display()).into());
    }

    println!(
        "  Checking {} for env imports...",
        wasm_file.display().to_string().dimmed()
    );

    let output = Command::new("wasm-objdump")
        .args(["-j", "Import", "-x"])
        .arg(&wasm_file)
        .output()?;

    if !output.status.success() {
        // wasm-objdump might not be installed, just warn
        println!(
            "  {}: wasm-objdump not found, skipping env import check",
            "Warning".yellow()
        );
        println!(
            "  Install wabt to enable this check: {}",
            "brew install wabt".dimmed()
        );
        return Ok(());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut env_imports = Vec::new();

    for line in stdout.lines() {
        if line.contains("<- env.") {
            env_imports.push(line.trim().to_string());
        }
    }

    if !env_imports.is_empty() {
        eprintln!(
            "\n  {}: Found {} env imports that won't work in browsers:",
            "ERROR".red().bold(),
            env_imports.len()
        );
        for import in &env_imports {
            eprintln!("    {}", import.red());
        }
        eprintln!();
        eprintln!("  These functions need to be provided in wasm-sysroot or avoided.");
        eprintln!("  Common causes:");
        eprintln!("    - Missing function in wasm-sysroot (add stub implementation)");
        eprintln!("    - External scanner using unavailable libc functions");
        eprintln!("    - Grammar not properly compiled for WASM target");
        return Err(format!("Found {} env imports in WASM", env_imports.len()).into());
    }

    println!(
        "  {} {}",
        "✓".green().bold(),
        "No env imports found - WASM is browser-compatible!".green()
    );
    Ok(())
}
