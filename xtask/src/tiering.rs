/// Group grammars by dependency level for parallel processing
/// Level 0: grammars with no dependencies
/// Level 1: grammars that depend only on level 0
/// etc.
fn group_by_dependency_level(
    grammars: &[PathBuf],
) -> Result<Vec<Vec<PathBuf>>, Box<dyn std::error::Error>> {
    let deps: HashMap<&str, &[&str]> = GRAMMAR_DEPS.iter().cloned().collect();

    // Build name -> path map
    let name_to_path: HashMap<String, &PathBuf> = grammars
        .iter()
        .map(|p| (p.file_name().unwrap().to_string_lossy().to_string(), p))
        .collect();

    let all_names: HashSet<String> = name_to_path.keys().cloned().collect();

    // Calculate level for each grammar
    fn get_level(
        name: &str,
        deps: &HashMap<&str, &[&str]>,
        all_names: &HashSet<String>,
        cache: &mut HashMap<String, usize>,
    ) -> usize {
        if let Some(&level) = cache.get(name) {
            return level;
        }

        let level = if let Some(grammar_deps) = deps.get(name) {
            let max_dep_level = grammar_deps
                .iter()
                .filter(|d| all_names.contains(**d))
                .map(|d| get_level(d, deps, all_names, cache))
                .max()
                .unwrap_or(0);
            max_dep_level + 1
        } else {
            0
        };

        cache.insert(name.to_string(), level);
        level
    }

    let mut level_cache = HashMap::new();
    let mut levels: HashMap<usize, Vec<PathBuf>> = HashMap::new();

    for name in &all_names {
        let level = get_level(name, &deps, &all_names, &mut level_cache);
        levels
            .entry(level)
            .or_default()
            .push(name_to_path[name].clone());
    }

    // Convert to vec of vecs, sorted by level
    let max_level = levels.keys().max().copied().unwrap_or(0);
    let mut result = Vec::new();
    for level in 0..=max_level {
        if let Some(mut grammars) = levels.remove(&level) {
            grammars.sort();
            result.push(grammars);
        }
    }

    Ok(result)
}
