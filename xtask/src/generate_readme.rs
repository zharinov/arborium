// =============================================================================
// README generation
// =============================================================================

/// Generate README.md from GRAMMARS.toml
fn generate_readme() {
    let repo_root = find_repo_root().expect("Could not find repo root");

    println!("Generating README.md from GRAMMARS.toml...\n");

    // Parse GRAMMARS.toml
    let grammars = match parse_grammars_toml(&repo_root) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Error parsing GRAMMARS.toml: {}", e);
            std::process::exit(1);
        }
    };

    // Group grammars by license type
    let mut permissive: Vec<&GrammarConfig> = Vec::new();
    let mut gpl: Vec<&GrammarConfig> = Vec::new();

    for grammar in grammars.values() {
        if grammar.license.contains("GPL") {
            gpl.push(grammar);
        } else {
            permissive.push(grammar);
        }
    }

    // Sort by name
    permissive.sort_by(|a, b| a.name.cmp(&b.name));
    gpl.sort_by(|a, b| a.name.cmp(&b.name));

    let total_count = grammars.len();
    let permissive_count = permissive.len();
    let gpl_count = gpl.len();

    // Generate permissive grammars table
    let mut permissive_table = String::new();
    permissive_table.push_str("| Feature | Language | License | Source |\n");
    permissive_table.push_str("|---------|----------|---------|--------|\n");
    for grammar in &permissive {
        let feature = format!("`lang-{}`", grammar.name);
        let display_name = grammar_display_name(&grammar.name);
        let repo_link = if grammar.repo == "local" {
            "local".to_string()
        } else {
            format!("[tree-sitter-{}]({})", grammar.name, grammar.repo)
        };
        permissive_table.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            feature, display_name, grammar.license, repo_link
        ));
    }

    // Generate GPL grammars table
    let mut gpl_table = String::new();
    gpl_table.push_str("| Feature | Language | License | Source |\n");
    gpl_table.push_str("|---------|----------|---------|--------|\n");
    for grammar in &gpl {
        let feature = format!("`lang-{}`", grammar.name);
        let display_name = grammar_display_name(&grammar.name);
        let repo_link = format!("[tree-sitter-{}]({})", grammar.name, grammar.repo);
        gpl_table.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            feature, display_name, grammar.license, repo_link
        ));
    }

    // Read template
    let template_path = repo_root.join("README.template.md");
    let template = match fs::read_to_string(&template_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error reading README.template.md: {}", e);
            eprintln!("Please create README.template.md with placeholders:");
            eprintln!("  {{{{TOTAL_COUNT}}}} - total number of grammars");
            eprintln!("  {{{{PERMISSIVE_COUNT}}}} - number of permissive grammars");
            eprintln!("  {{{{GPL_COUNT}}}} - number of GPL grammars");
            eprintln!("  {{{{PERMISSIVE_TABLE}}}} - table of permissive grammars");
            eprintln!("  {{{{GPL_TABLE}}}} - table of GPL grammars");
            std::process::exit(1);
        }
    };

    // Replace placeholders
    let output = template
        .replace("{{TOTAL_COUNT}}", &total_count.to_string())
        .replace("{{PERMISSIVE_COUNT}}", &permissive_count.to_string())
        .replace("{{GPL_COUNT}}", &gpl_count.to_string())
        .replace("{{PERMISSIVE_TABLE}}", permissive_table.trim())
        .replace("{{GPL_TABLE}}", gpl_table.trim());

    // Write output
    let output_path = repo_root.join("README.md");
    match fs::write(&output_path, &output) {
        Ok(_) => {
            println!("  Generated README.md:");
            println!("    - {} total grammars", total_count);
            println!(
                "    - {} permissive (MIT/Apache/CC0/Unlicense)",
                permissive_count
            );
            println!("    - {} GPL-licensed", gpl_count);
        }
        Err(e) => {
            eprintln!("Error writing README.md: {}", e);
            std::process::exit(1);
        }
    }

    println!("\nDone!");
}
