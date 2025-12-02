/// Pre-compress compressible files in the demo directory (in parallel)
fn precompress_files(demo_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use std::sync::{Arc, Mutex};
    use std::thread;

    let files_to_compress = [
        "index.html",
        "pkg/arborium_demo.js",
        "pkg/arborium_demo_bg.wasm",
    ];

    let results: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let mut handles = Vec::new();

    for file in files_to_compress {
        let file_path = demo_dir.join(file);
        if !file_path.exists() {
            continue;
        }

        let data = fs::read(&file_path)?;
        let raw_size = data.len();
        let file_name = file.to_string();

        // Spawn gzip compression
        {
            let results = Arc::clone(&results);
            let data = data.clone();
            let file_path = file_path.clone();
            let file_name = file_name.clone();
            handles.push(thread::spawn(move || {
                let gz_path = PathBuf::from(format!("{}.gz", file_path.display()));
                let gz_data = compress_gzip(&data).expect("gzip compression failed");
                fs::write(&gz_path, &gz_data).expect("failed to write .gz file");
                let msg = format!(
                    "  {} ({}) -> .gz ({})",
                    file_name,
                    format_size(raw_size),
                    format_size(gz_data.len())
                );
                results.lock().unwrap().push(msg);
            }));
        }

        // Spawn brotli compression in parallel
        {
            let results = Arc::clone(&results);
            handles.push(thread::spawn(move || {
                let br_path = PathBuf::from(format!("{}.br", file_path.display()));
                let br_data = compress_brotli(&data).expect("brotli compression failed");
                fs::write(&br_path, &br_data).expect("failed to write .br file");
                let msg = format!(
                    "  {} ({}) -> .br ({})",
                    file_name,
                    format_size(raw_size),
                    format_size(br_data.len())
                );
                results.lock().unwrap().push(msg);
            }));
        }
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("compression thread panicked");
    }

    // Print results
    let results = results.lock().unwrap();
    for msg in results.iter() {
        println!("{}", msg);
    }

    Ok(())
}

/// Pre-compress files with fast (low quality) settings for dev mode
fn precompress_files_fast(demo_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use std::sync::{Arc, Mutex};
    use std::thread;

    let files_to_compress = [
        "index.html",
        "pkg/arborium_demo.js",
        "pkg/arborium_demo_bg.wasm",
    ];

    let results: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let mut handles = Vec::new();

    for file in files_to_compress {
        let file_path = demo_dir.join(file);
        if !file_path.exists() {
            continue;
        }

        let data = fs::read(&file_path)?;
        let raw_size = data.len();
        let file_name = file.to_string();

        // Spawn fast gzip compression
        {
            let results = Arc::clone(&results);
            let data = data.clone();
            let file_path = file_path.clone();
            let file_name = file_name.clone();
            handles.push(thread::spawn(move || {
                let gz_path = PathBuf::from(format!("{}.gz", file_path.display()));
                let gz_data = compress_gzip_fast(&data).expect("gzip compression failed");
                fs::write(&gz_path, &gz_data).expect("failed to write .gz file");
                let msg = format!(
                    "  {} {} → {} {}",
                    file_name,
                    format!("({})", format_size(raw_size)).dimmed(),
                    ".gz".cyan(),
                    format!("({})", format_size(gz_data.len())).green()
                );
                results.lock().unwrap().push(msg);
            }));
        }

        // Spawn fast brotli compression in parallel
        {
            let results = Arc::clone(&results);
            handles.push(thread::spawn(move || {
                let br_path = PathBuf::from(format!("{}.br", file_path.display()));
                let br_data = compress_brotli_fast(&data).expect("brotli compression failed");
                fs::write(&br_path, &br_data).expect("failed to write .br file");
                let msg = format!(
                    "  {} {} → {} {}",
                    file_name,
                    format!("({})", format_size(raw_size)).dimmed(),
                    ".br".magenta(),
                    format!("({})", format_size(br_data.len())).green()
                );
                results.lock().unwrap().push(msg);
            }));
        }
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().expect("compression thread panicked");
    }

    // Print results
    let results = results.lock().unwrap();
    for msg in results.iter() {
        println!("{}", msg);
    }

    Ok(())
}

/// Compress data using brotli level 5 (good balance of speed and compression)
fn compress_brotli(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    let mut output = Vec::new();
    {
        let mut encoder = brotli::CompressorWriter::new(&mut output, 4096, 5, 22);
        encoder.write_all(data)?;
    }
    Ok(output)
}

/// Compress data using gzip (flate2) - best compression
fn compress_gzip(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    use flate2::Compression;
    use flate2::write::GzEncoder;

    let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(data)?;
    encoder.finish()
}

/// Compress data using brotli level 1 (fast)
fn compress_brotli_fast(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    let mut output = Vec::new();
    {
        let mut encoder = brotli::CompressorWriter::new(&mut output, 4096, 1, 22);
        encoder.write_all(data)?;
    }
    Ok(output)
}

/// Compress data using gzip level 1 (fast)
fn compress_gzip_fast(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    use flate2::Compression;
    use flate2::write::GzEncoder;

    let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
    encoder.write_all(data)?;
    encoder.finish()
}
