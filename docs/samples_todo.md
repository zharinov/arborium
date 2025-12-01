# Sample Sourcing TODO

Goal: add real-world, permissively licensed sample files to each grammar’s `info.toml` under `[[samples]]` with attribution (`path`, `description`, `link`, `license`).

## Done
- asm: NASM test `test/struc.asm` (BSD-2-Clause) → `crates/arborium-asm/samples/struc.asm`
- bash: Bash3Boilerplate `main.sh` (MIT) → `crates/arborium-bash/samples/bash3boilerplate_main.sh`
- c: `portfoliocourses/c-example-code/queue_linked_list.c` (MIT) → `crates/arborium-c/samples/queue_linked_list.c`
- java: Spring Petclinic `Vet.java` (Apache-2.0) → `crates/arborium-java/samples/Vet.java`
- python: Rich `examples/dynamic_progress.py` (MIT) → `crates/arborium-python/samples/dynamic_progress.py`
- typescript: TS conformance `discriminatedUnionTypes4.ts` (Apache-2.0) → `crates/arborium-typescript/samples/discriminatedUnionTypes4.ts`
- yaml: Kubernetes Deployment manifest (Apache-2.0) → `crates/arborium-yaml/samples/deployment.yaml`
- go: go/types interface-checker example (BSD-3) → `crates/arborium-go/samples/implements.go`
- rust: Tokio async chat example (MIT/Apache-2.0) → `crates/arborium-rust/samples/chat.rs`
- sql: PostgreSQL regression `join.sql` (PostgreSQL) → `crates/arborium-sql/samples/join.sql`

## Still TODO (need permissive samples)
- Remaining languages without `[[samples]]` blocks populated.
- Target next batch: c-sharp, clojure, cpp, css, dart, diff, dockerfile, elixir, elm, gleam, fsharp, haskell, html, idris, ini, jinja2, json, lean, lua, markdown, meson, nix, perl, php, powershell, rescript, ron, r, scala, scss, svelte, thrift, capnp, postscript, tlaplus, prolog, zsh, vue, glsl, hcl, nginx, devicetree, starlark, wasm, x86asm, xml, zig.

## Guidance for adding more
1. Pick one real-world file per language from MIT/Apache/BSD/CC0 repos (avoid trivial “hello world”).  
2. Vendor it under `crates/arborium-<lang>/samples/` and add a `[[samples]]` block with:
   - `path` (relative in this repo)  
   - `description` (what the sample demonstrates)  
   - `link` (upstream source URL)  
   - `license` (upstream license)  
3. Include license text if required by the upstream license.
