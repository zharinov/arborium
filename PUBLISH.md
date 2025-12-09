# Publishing Guide


## Core Crates (always present)

```
crates/
├── arborium/                  ← main crate with inventory
├── tree-sitter/               ← fork to avoid upstream build errors
└── tree-sitter-highlight/     ← fork to avoid upstream build errors
```

- These are always in the repo (not generated) and are published every release.
- The tree-sitter forks track upstream but include fixes so CI builds reliably across targets.

## Release Flow (current CI)

Everything is generated **once**, then a single tag publishes all crates + npm packages.

### 1. Prepare a release locally (optional but recommended)

```bash
# Pick a version and generate everything (core + all groups)
# This runs tree-sitter-cli for all grammars, but results are cached
# by tree-sitter-cli version + grammar.js hash, so reruns are cheap.

cargo xtask gen --version 0.3.0
```

`xtask gen` also records the current release version in a small metadata file.
CI will regenerate from source of truth as well, but running this locally lets
you review changes before tagging.

### 2. Tag and push (single tag)

```bash
git commit -am "Release v0.3.0"
git tag v0.3.0
git push origin main --tags
```

The `v0.3.0` tag triggers the CI workflow in `.github/workflows/ci.yml` which:

- Regenerates all grammars and crates with `arborium-xtask gen --version 0.3.0`
- Builds and tests the `arborium` crate
- Builds all WASM plugins into `dist/plugins`
- Publishes:
  - All grammar crates and core crates to crates.io via `cargo xtask publish crates`
  - All npm packages (per-language + `@arborium/arborium`) via `cargo xtask publish npm -o dist/plugins`

## Two Outputs, Two Registries

### 1. Native Rust Crates → crates.io

- ~98 grammar crates organized into ~10 hand-picked animal groups
- Core crates (`arborium`, `arborium-collection`, `miette-arborium`, `tree-sitter-*`) always published
- Each group publishes independently from `langs/group-{animal}/` via
  `cargo ws publish --publish-as-is`
- We use `cargo ws publish` instead of `cargo publish --workspace` because the
  latter is still brittle with partial publishes; `cargo ws publish` can resume
  cleanly.
- **Retry-safe**: crates.io warns and skips already-published versions

### 2. WASM Plugins → npm

- All grammars with `generate-component: true` in
  `langs/group-{animal}/{lang}/def/arborium.kdl`
- Built via `cargo-component` for `wasm32-wasip2` from the same group directory
- Transpiled via `jco` for browser compatibility
- Published as per-language packages under the `@arborium` scope, e.g.
  `@arborium/rust`, `@arborium/javascript`, etc.
- **Published together with crates.io** in the same per-group CI job for version sync

## Publishing Strategy

- A single tag (`vX.Y.Z`) publishes **all** crates.io + npm artifacts together.
- Core crates (`arborium`, `tree-sitter-*`, `miette-arborium`, etc.) and all
  `arborium-{lang}` crates publish once per release.

### crates.io (all crates)

Cargo handles already-published versions gracefully - it warns and continues:
```
warning: crate arborium-rust@0.3.0 already exists on crates.io
```

So retrying a publish job is safe; already-published crates are skipped.

### npm (all packages, via xtask)

npm is **not graceful** - it hard-fails with `EPUBLISHCONFLICT`:
```
npm ERR! code EPUBLISHCONFLICT
npm ERR! Cannot publish over existing version
```

**xtask publish** (per group) must:
- Check if version exists before publishing
- Distinguish `EPUBLISHCONFLICT` (skip, continue) from real errors (fail)
- Handle retries without re-publishing successes

The CI workflow builds all plugins into `dist/plugins` first, then one job
calls `xtask publish npm -o dist/plugins` to publish every npm package in one
go.

`xtask publish npm` reads the canonical version from `version.json` (written by
`xtask gen --version`) and enforces:

- Refuses to publish if the version is a dev one like `0.0.0-dev`
- Refuses to publish if any npm package version does not match `version.json`

This means you *must* run `cargo xtask gen --version X.Y.Z` before publishing
to npm, otherwise the command will fail instead of silently pushing the wrong
version (e.g., `0.0.0-dev`) to the registry.

## What's in Git vs Generated

### Source of Truth vs Generated

The **source of truth** for each language lives under its group and language,
inside a `def/` directory. Generated artifacts live next to it as `crate/`
(static Rust crate) and `npm/` (WASM plugin package).

```
langs/
├── group-squirrel/               (Web languages)
│   ├── rust/
│   │   ├── def/                  ← LANGUAGE DEFINITIONS (committed)
│   │   │   ├── arborium.kdl      ← SOURCE OF TRUTH
│   │   │   ├── grammar/
│   │   │   │   ├── grammar.js    ← tree-sitter grammar
│   │   │   │   └── scanner.c     ← custom scanner (if any)
│   │   │   ├── queries/
│   │   │   │   └── highlights.scm ← highlight queries
│   │   │   └── samples/          ← test samples
│   │   ├── crate/                ← Static linking crate (generated)
│   │   └── npm/                  ← WASM plugin package (generated)
│   ├── javascript/
│   ├── html/
│   └── [other web languages...]
├── group-deer/                   (C family)
│   ├── c/
│   ├── cpp/
│   ├── objc/
│   └── [other C family languages...]
├── group-fox/                    (Systems languages)
│   ├── python/
│   ├── go/
│   ├── java/
│   └── [other systems languages...]
├── group-bear/                   (Web frameworks)
│   ├── typescript/
│   ├── tsx/
│   ├── svelte/
│   ├── vue/
│   └── [other web frameworks...]
├── group-wolf/                   (Data/config)
│   ├── json/
│   ├── yaml/
│   ├── toml/
│   ├── xml/
│   └── [other data formats...]
└── group-otter/                  (Scripting/other)
    ├── bash/
    ├── perl/
    ├── php/
    ├── ruby/
    └── [other scripting languages...]
```
### Generated (gitignored)

```
langs/group-{animal}/{lang}/crate/
├── Cargo.toml            ← GENERATED by xtask gen
├── build.rs              ← GENERATED by xtask gen
├── src/lib.rs            ← GENERATED by xtask gen
└── grammar/
    └── src/              ← GENERATED by xtask gen (tree-sitter generate)
        ├── parser.c
        ├── grammar.json
        └── ...

langs/group-{animal}/{lang}/npm/
├── Cargo.toml            ← GENERATED for cargo-component
├── src/
│   └── bindings.rs      ← GENERATED bindings
└── package.json          ← GENERATED npm package
```

### Non-generated crates (hand-written, committed)

These crates don't have `arborium.kdl` and are fully hand-written:
- `arborium` (main crate)
- `arborium-test-harness`
- `arborium-sysroot`
- `arborium-host`
- `arborium-wire`
- `arborium-plugin-runtime`
- `miette-arborium`

## What `xtask gen --version X.Y.Z` Does

1. **Updates core crate versions:**
   - `arborium/Cargo.toml` version = "X.Y.Z"
   - `arborium-collection/Cargo.toml` version = "X.Y.Z"

2. **Generates group workspace files:**
   - `langs/group-{animal}/Cargo.toml` with member crates and version "X.Y.Z"

3. **Generates grammar crate files from definitions:**
   - Reads `langs/group-{animal}/{lang}/def/arborium.kdl` and friends
   - Writes `langs/group-{animal}/{lang}/crate/Cargo.toml` with version "X.Y.Z"
   - Writes `build.rs` with correct C compilation setup
   - Writes `src/lib.rs` with language exports
   - Runs tree-sitter generate into `grammar/src/*`

4. **Generates WASM plugin packages:**
   - Writes `langs/group-{animal}/{lang}/npm/Cargo.toml` for cargo-component build
   - Writes `langs/group-{animal}/{lang}/npm/package.json` for npm publishing
   - Writes `langs/group-{animal}/{lang}/npm/src/bindings.rs` generated bindings

When called without `--version`, uses `0.0.0-dev` (fine for local dev since path deps ignore versions).

## Workflows

### Local Development

```bash
# Edit arborium.kdl, grammar.js, queries, etc.

# Regenerate (uses 0.0.0-dev version, doesn't matter locally)
cargo xtask gen

# Build and test
cargo build
cargo test
```

### Release

See **Release Flow** above for the full tag + CI story.

## Artifacts Published

| Registry | Package | Count |
|----------|---------|-------|
| crates.io | `arborium` (core with inventory) | 1 |
| crates.io | `arborium-collection` (feature-gated) | 1 |
| crates.io | `arborium-{lang}` (static crates) | 98 |
| crates.io | `arborium-test-harness` | 1 |
| crates.io | `arborium-sysroot` | 1 |
| crates.io | `tree-sitter-patched-arborium` | 1 |
| crates.io | `tree-sitter-highlight-patched-arborium` | 1 |
| crates.io | `miette-arborium` | 1 |
| npmjs.com | `@arborium/arborium` (bundle) | 1 |
| npmjs.com | `@arborium/{lang}` (per-language WASM plugins) | 98 |

## Integrations

These projects use arborium for syntax highlighting:

| Integration | Description | Status |
|------------|-------------|--------|
| **miette-arborium** | Syntax highlighting for [miette](https://crates.io/crates/miette) error diagnostics | Published |
| **dodeca** | Static site generator ([website](https://dodeca.bearcove.eu/), [GitHub](https://github.com/bearcove/dodeca)) | In use |
| **docs.rs** | Via `--html-in-header` for TOML/shell highlighting in rustdoc | Documented |

## Roadmap to 1.0

### Done

- [x] Core Rust library with 69+ language grammars
- [x] Theme system with 15+ bundled themes (Catppuccin, Dracula, Tokyo Night, etc.)
- [x] HTML rendering with compact custom elements (`<a-k>` vs `<span class="keyword">`)
- [x] ANSI output for terminal applications
- [x] miette integration for error reporting
- [x] Browser IIFE drop-in script (auto-highlights code blocks)
- [x] Browser ESM module for bundlers
- [x] WASM compilation target (`wasm32-unknown-unknown`)
- [x] docs.rs integration via `--html-in-header`
- [x] IIFE skips already-highlighted blocks (docs.rs compatibility)

### In Progress

- [ ] WASM component plugin system (Phase 4 of WASMPLUG.md)
  - [x] WIT interfaces defined (`grammar.wit`, `host.wit`)
  - [x] Plugin runtime crate (`arborium-plugin-runtime`)
  - [x] Host component started (`arborium-host`)
  - [ ] Dynamic grammar loading from bytes
  - [ ] Injection dependency resolution in browser

### Blocking 1.0

- [ ] **Language injection in browser** - HTML→JS→SQL nesting works in Rust but
      not yet in WASM host component
- [ ] **Visual regression tests** - Playwright infrastructure exists but needs
      integration with CI
- [ ] **API stability review** - Document public API surface, deprecation policy

### Nice to Have (post-1.0)

- [ ] Standalone CLI tool for highlighting files
- [ ] Plugin discovery/registry mechanism (dynamic grammar loading)
- [ ] Performance benchmarks and optimization pass
- [ ] Incremental parsing with `apply-edit` support
- [ ] Sample files for all ~98 grammars (see `docs/samples_todo.md`)

## Publishing TODO

- [ ] Finalize ~10 hand-crafted animal groups (squirrel, deer, fox, bear, wolf,
      otter, …) and document which languages live where
- [ ] Implement inventory system in arborium crate
- [ ] Create arborium-collection crate with feature flags
- [ ] Update `xtask publish` + `xtask tag` commands for:
  - [ ] Group-based publishing (per-animal tags like v0.3.0-squirrel)
  - [ ] Combined crates.io + npm publishing per group
  - [ ] Inventory-aware dependency resolution
- [ ] Update generate caching to tree-sitter-cli output only
- [ ] Standardize wasm-opt settings to -Oz
- [ ] Unify release.yml and npm-publish.yml into single workflow
