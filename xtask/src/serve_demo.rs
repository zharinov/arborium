// =============================================================================
// Demo server
// =============================================================================

/// Build and serve the WASM demo
fn serve_demo(addr: &str, specified_port: Option<u16>, dev_mode: bool) {
    let repo_root = find_repo_root().expect("Could not find repo root");
    let demo_dir = repo_root.join("demo");

    if dev_mode {
        println!("Building and serving arborium demo (DEV MODE)...\n");
    } else {
        println!("Building and serving arborium demo...\n");
    }

    // Step 0: Generate index.html from template (use full generate_demo)
    println!(
        "\n{} {}",
        "==>".cyan().bold(),
        "Generating demo HTML".bold()
    );
    generate_demo();

    // Step 1: Check for wasm-pack
    step("Checking for wasm-pack", || ensure_wasm_pack());

    if dev_mode {
        // DEV MODE: Fast build with --dev flag, skip wasm-opt
        step("Building WASM package (wasm-pack --dev)", || {
            println!("  Running wasm-pack build --dev --target web...");
            let status = Command::new("wasm-pack")
                .args(["build", "--dev", "--target", "web"])
                .current_dir(&demo_dir)
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()?;

            if !status.success() {
                return Err("wasm-pack build failed".into());
            }

            println!("  Dev build complete!");
            Ok(())
        });

        // Check for env imports
        step("Checking for env imports", || {
            let wasm_file = demo_dir.join("pkg/arborium_demo_bg.wasm");
            check_wasm_env_imports(&wasm_file)
        });

        // Fast compression with low quality settings
        step("Pre-compressing files (fast)", || {
            precompress_files_fast(&demo_dir)
        });
    } else {
        // RELEASE MODE: Full optimizations

        // Step 2: Cargo build in debug mode (fast, no optimizations) for early env import check
        step("Building WASM (debug, fast)", || {
            println!("  Running cargo build (debug) for WASM target...");
            let status = Command::new("cargo")
                .args([
                    "build",
                    "--target",
                    "wasm32-unknown-unknown",
                    "-p",
                    "arborium-demo",
                ])
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()?;

            if !status.success() {
                return Err("cargo build failed".into());
            }

            println!("  Debug build complete!");
            Ok(())
        });

        // Step 3: Check for env imports on debug build (before slow optimized build)
        step("Checking for env imports", || {
            check_wasm_env_imports_debug()
        });

        // Step 4: Build the WASM package with wasm-pack (includes wasm-opt)
        step("Building WASM package (wasm-pack)", || {
            println!("  Running wasm-pack build --target web...");
            let status = Command::new("wasm-pack")
                .args(["build", "--target", "web"])
                .current_dir(&demo_dir)
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()?;

            if !status.success() {
                return Err("wasm-pack build failed".into());
            }

            println!("  Build complete!");
            Ok(())
        });

        // Step 5: Verify env imports on final release build
        step("Verifying release build (env imports)", || {
            let wasm_file = demo_dir.join("pkg/arborium_demo_bg.wasm");
            check_wasm_env_imports(&wasm_file)
        });

        // Step 6: Pre-compress files with best compression
        step("Pre-compressing files", || precompress_files(&demo_dir));
    }

    // Step 7: Start HTTP server
    println!(
        "\n{} {}",
        "==>".cyan().bold(),
        "Starting HTTP server".bold()
    );
    println!(
        "  Demo directory: {}",
        demo_dir.display().to_string().dimmed()
    );
    println!();

    let (server, port) = if let Some(p) = specified_port {
        // Try the specified port only
        match tiny_http::Server::http(format!("{}:{}", addr, p)) {
            Ok(s) => (s, p),
            Err(e) => {
                eprintln!("Error: Could not bind to {}:{}: {}", addr, p, e);
                std::process::exit(1);
            }
        }
    } else {
        // Try ports 8000-8010
        let mut server = None;
        let mut port = 8000u16;

        while port <= 8010 {
            match tiny_http::Server::http(format!("{}:{}", addr, port)) {
                Ok(s) => {
                    server = Some(s);
                    break;
                }
                Err(_) => {
                    port += 1;
                }
            }
        }

        match server {
            Some(s) => (s, port),
            None => {
                eprintln!(
                    "Error: Could not bind to any port between 8000-8010 on {}",
                    addr
                );
                std::process::exit(1);
            }
        }
    };

    let display_addr = if addr == "0.0.0.0" { "localhost" } else { addr };
    let url = format!("http://{}:{}", display_addr, port);
    println!();
    println!(
        "  {} {}",
        "✓".green().bold(),
        "Demo server ready!".green().bold()
    );
    println!();
    println!("    {} {}", "→".cyan(), url.cyan().bold().underline());
    println!();
    println!("    {}", "Press Ctrl+C to stop".dimmed());
    println!();

    // Compute bundle info once at startup
    let bundle_info = compute_bundle_info(&demo_dir);

    // Serve files
    for request in server.incoming_requests() {
        let url_path = request.url().trim_start_matches('/');

        // Special endpoint for bundle info
        if url_path == "bundle-info.json" {
            let response = tiny_http::Response::from_string(&bundle_info).with_header(
                tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"application/json"[..])
                    .unwrap(),
            );
            let _ = request.respond(response);
            continue;
        }

        let file_path = if url_path.is_empty() || url_path == "/" {
            demo_dir.join("index.html")
        } else {
            demo_dir.join(url_path)
        };

        // Security: ensure path is within demo_dir
        let file_path = match file_path.canonicalize() {
            Ok(p) => p,
            Err(_) => {
                let response = tiny_http::Response::from_string("Not Found").with_status_code(404);
                let _ = request.respond(response);
                continue;
            }
        };

        if !file_path.starts_with(&demo_dir) {
            let response = tiny_http::Response::from_string("Forbidden").with_status_code(403);
            let _ = request.respond(response);
            continue;
        }

        // Check what encodings the client accepts
        let accept_encoding = request
            .headers()
            .iter()
            .find(|h| h.field.as_str().to_ascii_lowercase() == "accept-encoding")
            .map(|h| h.value.as_str().to_string())
            .unwrap_or_default();

        let accepts_br = accept_encoding.contains("br");
        let accepts_gzip = accept_encoding.contains("gzip");

        // Try to serve pre-compressed files (prefer brotli over gzip)
        let br_path = PathBuf::from(format!("{}.br", file_path.display()));
        let gz_path = PathBuf::from(format!("{}.gz", file_path.display()));
        let (serve_path, encoding) = if accepts_br && br_path.exists() {
            (br_path, Some("br"))
        } else if accepts_gzip && gz_path.exists() {
            (gz_path, Some("gzip"))
        } else {
            (file_path.clone(), None)
        };

        // Read and serve the file
        match fs::read(&serve_path) {
            Ok(content) => {
                let content_type = guess_content_type(&file_path); // Use original path for content-type

                let mut response = tiny_http::Response::from_data(content).with_header(
                    tiny_http::Header::from_bytes(&b"Content-Type"[..], content_type.as_bytes())
                        .unwrap(),
                );

                if let Some(enc) = encoding {
                    response = response.with_header(
                        tiny_http::Header::from_bytes(&b"Content-Encoding"[..], enc.as_bytes())
                            .unwrap(),
                    );
                }

                let _ = request.respond(response);
            }
            Err(_) => {
                let response = tiny_http::Response::from_string("Not Found").with_status_code(404);
                let _ = request.respond(response);
            }
        }
    }
}
