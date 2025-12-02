/// Generate demo files from templates
fn generate_demo() {
    use serde_json::Value;
    use std::collections::HashSet;

    let repo_root = find_repo_root().expect("Could not find repo root");
    let demo_dir = repo_root.join("demo");
    let template_path = demo_dir.join("template.html");
    let app_js_path = demo_dir.join("app.js");
    let styles_css_path = demo_dir.join("styles.css");
    let output_html = demo_dir.join("index.html");
    let output_js = demo_dir.join("pkg").join("app.generated.js");

    println!("Generating demo files...\n");

    // Step 1: Build language info from info.toml files
    println!("  Reading language info from info.toml files...");
    let crates_dir = repo_root.join("crates");
    let mut lang_info: serde_json::Map<String, Value> = serde_json::Map::new();
    let mut crate_samples: BTreeMap<String, PathBuf> = BTreeMap::new(); // lang_id -> sample file path

    // Find all arborium-* crates
    if let Ok(entries) = fs::read_dir(&crates_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let dir_name = path.file_name().unwrap().to_string_lossy();
            if !dir_name.starts_with("arborium-") {
                continue;
            }
            let lang_id = dir_name.strip_prefix("arborium-").unwrap().to_string();

            let info_toml = path.join("info.toml");
            if !info_toml.exists() {
                continue;
            }

            if let Ok(content) = fs::read_to_string(&info_toml) {
                // Parse language info
                if let Some(language_info) = config::parse_language_info(&content) {
                    // Parse sample info
                    let sample_info = config::parse_sample_info(&content);

                    // Store sample path for later
                    if let Some(ref sample) = sample_info {
                        if let Some(ref sample_path_str) = sample.path {
                            let sample_path = path.join(sample_path_str);
                            if sample_path.exists() {
                                crate_samples.insert(lang_id.clone(), sample_path);
                            }
                        }
                    }

                    // Convert to JSON
                    let json = config::language_info_to_json(&language_info, sample_info.as_ref());
                    lang_info.insert(lang_id.clone(), json);
                }
            }
        }
    }
    println!("    Found {} languages", lang_info.len());

    // Serialize language info
    let lang_info_value = Value::Object(lang_info.clone());
    let lang_info_str =
        serde_json::to_string(&lang_info_value).expect("Failed to serialize lang_info");

    // Step 2: Collect all unique icon names
    println!("  Collecting icons...");
    let mut icon_names: HashSet<String> = HashSet::new();

    // Add icons from language info
    for (_lang_id, info) in &lang_info {
        if let Some(icon) = info.get("icon").and_then(|i| i.as_str()) {
            icon_names.insert(icon.to_string());
        }
    }

    // Add icons used in template.html (e.g., {{ICON:mdi:tree}})
    let template = match fs::read_to_string(&template_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error reading template.html: {}", e);
            std::process::exit(1);
        }
    };

    // Find all {{ICON:xxx}} patterns
    let icon_pattern = regex::Regex::new(r"\{\{ICON:([^}]+)\}\}").unwrap();
    for cap in icon_pattern.captures_iter(&template) {
        if let Some(icon) = cap.get(1) {
            icon_names.insert(icon.as_str().to_string());
        }
    }

    // Also add a fallback icon
    icon_names.insert("mdi:code-tags".to_string());

    println!("    Found {} unique icons", icon_names.len());

    // Step 3: Fetch SVGs from Iconify API
    println!("  Fetching icons from Iconify API...");
    let mut icons: BTreeMap<String, String> = BTreeMap::new();

    // Check for cached icons
    let cache_path = demo_dir.join(".icon-cache.json");
    let mut cached_icons: BTreeMap<String, String> = if cache_path.exists() {
        match fs::read_to_string(&cache_path) {
            Ok(s) => serde_json::from_str(&s).unwrap_or_default(),
            Err(_) => BTreeMap::new(),
        }
    } else {
        BTreeMap::new()
    };

    let mut fetch_count = 0;
    for icon_name in &icon_names {
        // Check cache first
        if let Some(svg) = cached_icons.get(icon_name) {
            icons.insert(icon_name.clone(), svg.clone());
            continue;
        }

        // Parse icon name (prefix:name)
        let parts: Vec<&str> = icon_name.split(':').collect();
        if parts.len() != 2 {
            eprintln!("    Warning: Invalid icon format: {}", icon_name);
            continue;
        }
        let (prefix, name) = (parts[0], parts[1]);

        // Fetch from Iconify API
        let url = format!("https://api.iconify.design/{}/{}.svg", prefix, name);
        match ureq::get(&url).call() {
            Ok(resp) => {
                if let Ok(svg) = resp.into_string() {
                    // Clean up the SVG (remove unnecessary attributes)
                    let cleaned = svg
                        .replace("xmlns=\"http://www.w3.org/2000/svg\"", "")
                        .replace("xmlns:xlink=\"http://www.w3.org/1999/xlink\"", "");
                    icons.insert(icon_name.clone(), cleaned.clone());
                    cached_icons.insert(icon_name.clone(), cleaned);
                    fetch_count += 1;
                    print!(".");
                    std::io::stdout().flush().ok();
                }
            }
            Err(e) => {
                eprintln!("\n    Warning: Failed to fetch {}: {}", icon_name, e);
            }
        }
    }
    if fetch_count > 0 {
        println!();
    }
    println!(
        "    Fetched {} new icons, {} from cache",
        fetch_count,
        icons.len() - fetch_count
    );

    // Save cache
    if let Ok(cache_json) = serde_json::to_string_pretty(&cached_icons) {
        let _ = fs::write(&cache_path, cache_json);
    }

    // Step 4: Read sample files from crates
    println!("  Reading sample files from crates...");
    let mut examples: BTreeMap<String, String> = BTreeMap::new();

    for (lang_id, sample_path) in &crate_samples {
        if let Ok(content) = fs::read_to_string(sample_path) {
            let file_name = sample_path.file_name().unwrap().to_string_lossy();
            println!("    {} {}", lang_id, file_name.to_string().dimmed());
            examples.insert(lang_id.clone(), content);
        }
    }
    println!("    Total: {} sample files", examples.len());

    // Step 5: Build the icons JavaScript object
    let mut icons_js = String::from("{\n");
    for (i, (name, svg)) in icons.iter().enumerate() {
        let escaped_svg = escape_for_js(svg);
        icons_js.push_str(&format!("    \"{}\": \"{}\"", name, escaped_svg));
        if i < icons.len() - 1 {
            icons_js.push(',');
        }
        icons_js.push('\n');
    }
    icons_js.push('}');

    // Step 6: Build examples JavaScript object
    let mut examples_js = String::from("{\n");
    for (i, (lang_id, content)) in examples.iter().enumerate() {
        let escaped = escape_for_js(content);
        examples_js.push_str(&format!("    \"{}\": \"{}\"", lang_id, escaped));
        if i < examples.len() - 1 {
            examples_js.push(',');
        }
        examples_js.push('\n');
    }
    examples_js.push('}');

    // Step 7: Read app.js template and do replacements
    println!("  Processing app.js...");
    let app_js_template = match fs::read_to_string(&app_js_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error reading app.js: {}", e);
            std::process::exit(1);
        }
    };

    let app_js_output = app_js_template
        .replace(
            "// {{LANGUAGE_INFO}}",
            &format!("const languageInfo = {};", lang_info_str),
        )
        .replace("{{EXAMPLES}}", &examples_js)
        .replace("{{ICONS}}", &icons_js);

    // Step 8: Generate theme previews from themes/*.toml
    println!("  Generating theme previews...");
    let themes_dir = repo_root.join("themes");
    let mut theme_swatches_html = String::new();
    if themes_dir.exists() {
        let mut themes: Vec<_> = fs::read_dir(&themes_dir)
            .expect("Failed to read themes directory")
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "toml"))
            .collect();
        themes.sort_by_key(|e| e.path());

        for entry in themes {
            let path = entry.path();
            let content = fs::read_to_string(&path).expect("Failed to read theme file");
            let theme: toml::Value = content.parse().expect("Failed to parse theme TOML");

            let name = theme
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown");
            let colors = theme.get("colors").and_then(|v| v.as_table());

            if let Some(colors) = colors {
                let bg = colors.get("bg").and_then(|v| v.as_str()).unwrap_or("#000");
                let keyword = colors
                    .get("keyword")
                    .and_then(|v| v.as_str())
                    .unwrap_or("#fff");
                let function = colors
                    .get("function")
                    .and_then(|v| v.as_str())
                    .unwrap_or("#fff");
                let string = colors
                    .get("string")
                    .and_then(|v| v.as_str())
                    .unwrap_or("#fff");
                let number = colors
                    .get("number")
                    .and_then(|v| v.as_str())
                    .unwrap_or("#fff");
                let macro_color = colors
                    .get("macro")
                    .and_then(|v| v.as_str())
                    .unwrap_or("#fff");

                // Mini code preview with number, function call, and macro
                theme_swatches_html.push_str(&format!(
                    r#"<div class="theme-preview" title="{}">
                        <pre style="background: {}"><code><span style="color: {}">fn</span> <span style="color: {}">main</span>() {{
  <span style="color: {}">let</span> x = <span style="color: {}">42</span>;
  <span style="color: {}">println!</span>(<span style="color: {}">"hi"</span>);
}}</code></pre>
                        <span class="theme-name">{}</span>
                    </div>
"#,
                    name, bg, keyword, function, keyword, number, macro_color, string, name
                ));
            }
        }
        println!(
            "    Generated {} theme previews",
            themes_dir.read_dir().unwrap().count()
        );
    }

    // Step 9: Process template.html - replace {{ICON:xxx}} with inline SVGs
    println!("  Processing template.html...");
    let mut html_output = template.clone();
    html_output = html_output.replace("{{THEME_SWATCHES}}", &theme_swatches_html);
    for cap in icon_pattern.captures_iter(&template) {
        let full_match = cap.get(0).unwrap().as_str();
        let icon_name = cap.get(1).unwrap().as_str();
        if let Some(svg) = icons.get(icon_name) {
            html_output = html_output.replace(full_match, svg);
        } else {
            // Fallback to empty span
            html_output = html_output.replace(full_match, "");
        }
    }

    // Step 9: Write outputs
    // Create pkg directory if it doesn't exist
    let pkg_dir = demo_dir.join("pkg");
    if !pkg_dir.exists() {
        fs::create_dir_all(&pkg_dir).expect("Failed to create pkg directory");
    }

    // Write generated app.js
    match fs::write(&output_js, &app_js_output) {
        Ok(_) => println!("  Written {}", output_js.display()),
        Err(e) => {
            eprintln!("Error writing app.generated.js: {}", e);
            std::process::exit(1);
        }
    }

    // Write index.html
    match fs::write(&output_html, &html_output) {
        Ok(_) => println!("  Written {}", output_html.display()),
        Err(e) => {
            eprintln!("Error writing index.html: {}", e);
            std::process::exit(1);
        }
    }

    // Generate styles.css with theme CSS from arborium
    let pkg_styles = pkg_dir.join("styles.css");
    println!("  Generating styles.css with theme CSS...");

    let base_css = fs::read_to_string(&styles_css_path).expect("Failed to read styles.css");

    // Find the markers
    const START_MARKER: &str = "/* ==== GENERATED_THEME_CSS_START ==== */";
    const END_MARKER: &str = "/* ==== GENERATED_THEME_CSS_END ==== */";

    let start_pos = base_css
        .find(START_MARKER)
        .expect("Could not find GENERATED_THEME_CSS_START marker in styles.css");
    let end_pos = base_css
        .find(END_MARKER)
        .expect("Could not find GENERATED_THEME_CSS_END marker in styles.css");

    // Split the CSS at the markers
    let before_marker = &base_css[..start_pos];
    let after_marker = &base_css[end_pos + END_MARKER.len()..];

    // Generate theme CSS from arborium
    let mut theme_css = String::new();
    theme_css.push_str(START_MARKER);
    theme_css.push_str(
        "\n/* This section is auto-generated by xtask from arborium::theme::builtin */\n",
    );
    theme_css.push_str("/* Do not edit manually - changes will be overwritten */\n\n");

    // Theme name to CSS selector mapping (for backward compatibility with demo)
    let theme_mappings: &[(&str, &arborium::theme::Theme)] = &[
        ("mocha", arborium::theme::builtin::catppuccin_mocha()),
        ("latte", arborium::theme::builtin::catppuccin_latte()),
        ("frappe", arborium::theme::builtin::catppuccin_frappe()),
        (
            "macchiato",
            arborium::theme::builtin::catppuccin_macchiato(),
        ),
        ("tokyo-night", arborium::theme::builtin::tokyo_night()),
        ("dracula", arborium::theme::builtin::dracula()),
        ("monokai", arborium::theme::builtin::monokai()),
        ("github-dark", arborium::theme::builtin::github_dark()),
        ("github-light", arborium::theme::builtin::github_light()),
        ("one-dark", arborium::theme::builtin::one_dark()),
        ("nord", arborium::theme::builtin::nord()),
        ("gruvbox-dark", arborium::theme::builtin::gruvbox_dark()),
        ("gruvbox-light", arborium::theme::builtin::gruvbox_light()),
        (
            "kanagawa-dragon",
            arborium::theme::builtin::kanagawa_dragon(),
        ),
        ("rose-pine-moon", arborium::theme::builtin::rose_pine_moon()),
        ("ayu-dark", arborium::theme::builtin::ayu_dark()),
        ("ayu-light", arborium::theme::builtin::ayu_light()),
        ("solarized-dark", arborium::theme::builtin::solarized_dark()),
        (
            "solarized-light",
            arborium::theme::builtin::solarized_light(),
        ),
        (
            "ef-melissa-dark",
            arborium::theme::builtin::ef_melissa_dark(),
        ),
        ("melange-dark", arborium::theme::builtin::melange_dark()),
        ("melange-light", arborium::theme::builtin::melange_light()),
        ("light-owl", arborium::theme::builtin::light_owl()),
        ("lucius-light", arborium::theme::builtin::lucius_light()),
    ];

    // Generate CSS for default theme (catppuccin mocha - base styles)
    // Use :root selector to scope the default theme's global rules
    theme_css.push_str("/* Default theme (Catppuccin Mocha) */\n");
    theme_css.push_str(&arborium::theme::builtin::catppuccin_mocha().to_css(":root"));
    theme_css.push('\n');

    // Generate CSS for each theme with [data-theme="..."] selector
    for (name, theme) in theme_mappings {
        let selector = format!("[data-theme=\"{}\"]", name);
        theme_css.push_str(&theme.to_css(&selector));
        theme_css.push('\n');
    }

    theme_css.push_str(END_MARKER);

    // Combine the CSS
    let final_css = format!("{}{}{}", before_marker, theme_css, after_marker);

    match fs::write(&pkg_styles, &final_css) {
        Ok(_) => println!(
            "  Generated {} with {} themes ({} bytes)",
            pkg_styles.display(),
            theme_mappings.len(),
            final_css.len()
        ),
        Err(e) => {
            eprintln!("Error writing styles.css: {}", e);
            std::process::exit(1);
        }
    }

    // Note: demo/src/lib.rs is no longer auto-generated.
    // It now uses arborium's high-level Highlighter API which handles all languages internally.

    println!("\nDone!");
    println!("  HTML: {}", format_size(html_output.len()));
    println!("  JS: {}", format_size(app_js_output.len()));
}
