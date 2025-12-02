/// Check if wasm-pack is installed, install if not
fn ensure_wasm_pack() -> Result<(), Box<dyn std::error::Error>> {
    if command_exists("wasm-pack") {
        let output = Command::new("wasm-pack").arg("--version").output()?;
        let version = String::from_utf8_lossy(&output.stdout);
        println!("  Found: {}", version.trim());
        return Ok(());
    }

    println!("  wasm-pack not found, installing...");

    // Try cargo binstall first
    let binstall = Command::new("cargo")
        .args(["binstall", "-y", "wasm-pack"])
        .status();

    if binstall.is_ok() && binstall.unwrap().success() {
        println!("  Installed via cargo binstall");
        return Ok(());
    }

    // Fall back to cargo install
    println!("  Trying cargo install...");
    let install = Command::new("cargo")
        .args(["install", "wasm-pack"])
        .status()?;

    if !install.success() {
        return Err("Failed to install wasm-pack".into());
    }

    println!("  Installed via cargo install");
    Ok(())
}

/// Check if a command exists in PATH
fn command_exists(cmd: &str) -> bool {
    Command::new(cmd)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok()
}

fn ensure_tree_sitter_cli() -> Result<(), Box<dyn std::error::Error>> {
    // Check if tree-sitter is available
    let version = Command::new("tree-sitter")
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success());

    if let Some(output) = version {
        let version_str = String::from_utf8_lossy(&output.stdout);
        println!("  Found: {}", version_str.trim());
        return Ok(());
    }

    println!("  tree-sitter CLI not found, installing...");

    // Try cargo binstall first (faster)
    let binstall = Command::new("cargo")
        .args(["binstall", "-y", "tree-sitter-cli"])
        .status();

    if binstall.is_ok() && binstall.unwrap().success() {
        println!("  Installed via cargo binstall");
        return Ok(());
    }

    // Fall back to cargo install
    println!("  cargo binstall failed, trying cargo install...");
    let install = Command::new("cargo")
        .args(["install", "tree-sitter-cli"])
        .status()?;

    if !install.success() {
        return Err("Failed to install tree-sitter-cli".into());
    }

    println!("  Installed via cargo install");
    Ok(())
}
