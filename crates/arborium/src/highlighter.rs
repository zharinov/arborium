//! High-level syntax highlighting API with automatic language injection support.
//!
//! This module provides a simple, batteries-included API for syntax highlighting
//! that automatically handles language injections (e.g., CSS in HTML `<style>` tags,
//! JavaScript in `<script>` tags).
//!
//! # Example
//!
//! ```rust,ignore
//! use arborium::highlighter::Highlighter;
//!
//! let mut highlighter = Highlighter::new();
//! let html = highlighter.highlight_to_html("svelte", r#"
//!     <script>
//!         let name = "world";
//!     </script>
//!     <h1>Hello {name}!</h1>
//!     <style>
//!         h1 { color: red; }
//!     </style>
//! "#)?;
//! ```

use std::collections::HashMap;
use std::io::{self, Write};

use crate::tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlight};
use crate::tree_sitter_highlight::Highlighter as TsHighlighter;
use crate::highlights;
use crate::html;

/// Error type for highlighting operations
#[derive(Debug)]
pub enum HighlightError {
    /// The requested language is not supported
    UnsupportedLanguage(String),
    /// Error compiling the grammar's queries
    QueryError(String),
    /// Error during highlighting
    HighlightError(String),
    /// IO error during rendering
    IoError(io::Error),
}

impl std::fmt::Display for HighlightError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HighlightError::UnsupportedLanguage(lang) => write!(f, "Unsupported language: {}", lang),
            HighlightError::QueryError(e) => write!(f, "Query error: {}", e),
            HighlightError::HighlightError(e) => write!(f, "Highlight error: {}", e),
            HighlightError::IoError(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for HighlightError {}

impl From<io::Error> for HighlightError {
    fn from(e: io::Error) -> Self {
        HighlightError::IoError(e)
    }
}

/// High-level syntax highlighter with automatic injection support.
///
/// This struct manages highlight configurations for all enabled languages
/// and automatically handles language injections.
pub struct Highlighter {
    /// Language configurations - public for testing
    #[cfg(test)]
    pub(crate) configs: HashMap<&'static str, HighlightConfiguration>,
    #[cfg(not(test))]
    configs: HashMap<&'static str, HighlightConfiguration>,
    ts_highlighter: TsHighlighter,
}

impl Default for Highlighter {
    fn default() -> Self {
        Self::new()
    }
}

impl Highlighter {
    /// Create a new highlighter with configurations for all enabled languages.
    pub fn new() -> Self {
        let mut configs = HashMap::new();
        let names: Vec<String> = crate::HIGHLIGHT_NAMES.iter().map(|s| s.to_string()).collect();

        // Register all enabled languages
        macro_rules! register_lang {
            ($configs:expr, $names:expr, $feature:literal, $module:ident, $primary:literal $(, $alias:literal)*) => {
                #[cfg(feature = $feature)]
                {
                    if let Ok(mut config) = HighlightConfiguration::new(
                        crate::$module::language().into(),
                        $primary,
                        crate::$module::HIGHLIGHTS_QUERY,
                        crate::$module::INJECTIONS_QUERY,
                        crate::$module::LOCALS_QUERY,
                    ) {
                        config.configure(&$names);
                        $configs.insert($primary, config);
                    }
                }
            };
        }

        // Core languages needed for injections
        register_lang!(configs, names, "lang-javascript", lang_javascript, "javascript");
        register_lang!(configs, names, "lang-css", lang_css, "css");
        register_lang!(configs, names, "lang-typescript", lang_typescript, "typescript");

        // All other languages
        register_lang!(configs, names, "lang-ada", lang_ada, "ada");
        register_lang!(configs, names, "lang-agda", lang_agda, "agda");
        register_lang!(configs, names, "lang-asm", lang_asm, "asm");
        register_lang!(configs, names, "lang-awk", lang_awk, "awk");
        register_lang!(configs, names, "lang-bash", lang_bash, "bash");
        register_lang!(configs, names, "lang-batch", lang_batch, "batch");
        register_lang!(configs, names, "lang-c", lang_c, "c");
        register_lang!(configs, names, "lang-c-sharp", lang_c_sharp, "c-sharp");
        register_lang!(configs, names, "lang-caddy", lang_caddy, "caddy");
        register_lang!(configs, names, "lang-capnp", lang_capnp, "capnp");
        register_lang!(configs, names, "lang-clojure", lang_clojure, "clojure");
        register_lang!(configs, names, "lang-cmake", lang_cmake, "cmake");
        register_lang!(configs, names, "lang-commonlisp", lang_commonlisp, "commonlisp");
        register_lang!(configs, names, "lang-cpp", lang_cpp, "cpp");
        register_lang!(configs, names, "lang-d", lang_d, "d");
        register_lang!(configs, names, "lang-dart", lang_dart, "dart");
        register_lang!(configs, names, "lang-devicetree", lang_devicetree, "devicetree");
        register_lang!(configs, names, "lang-diff", lang_diff, "diff");
        register_lang!(configs, names, "lang-dockerfile", lang_dockerfile, "dockerfile");
        register_lang!(configs, names, "lang-dot", lang_dot, "dot");
        register_lang!(configs, names, "lang-elisp", lang_elisp, "elisp");
        register_lang!(configs, names, "lang-elixir", lang_elixir, "elixir");
        register_lang!(configs, names, "lang-elm", lang_elm, "elm");
        register_lang!(configs, names, "lang-erlang", lang_erlang, "erlang");
        register_lang!(configs, names, "lang-fish", lang_fish, "fish");
        register_lang!(configs, names, "lang-fsharp", lang_fsharp, "fsharp");
        register_lang!(configs, names, "lang-gleam", lang_gleam, "gleam");
        register_lang!(configs, names, "lang-glsl", lang_glsl, "glsl");
        register_lang!(configs, names, "lang-go", lang_go, "go");
        register_lang!(configs, names, "lang-graphql", lang_graphql, "graphql");
        register_lang!(configs, names, "lang-haskell", lang_haskell, "haskell");
        register_lang!(configs, names, "lang-hcl", lang_hcl, "hcl");
        register_lang!(configs, names, "lang-hlsl", lang_hlsl, "hlsl");
        register_lang!(configs, names, "lang-html", lang_html, "html");
        register_lang!(configs, names, "lang-ini", lang_ini, "ini");
        register_lang!(configs, names, "lang-java", lang_java, "java");
        register_lang!(configs, names, "lang-jinja2", lang_jinja2, "jinja2");
        register_lang!(configs, names, "lang-jq", lang_jq, "jq");
        register_lang!(configs, names, "lang-json", lang_json, "json");
        register_lang!(configs, names, "lang-julia", lang_julia, "julia");
        register_lang!(configs, names, "lang-kdl", lang_kdl, "kdl");
        register_lang!(configs, names, "lang-kotlin", lang_kotlin, "kotlin");
        register_lang!(configs, names, "lang-lean", lang_lean, "lean");
        register_lang!(configs, names, "lang-lua", lang_lua, "lua");
        register_lang!(configs, names, "lang-matlab", lang_matlab, "matlab");
        register_lang!(configs, names, "lang-meson", lang_meson, "meson");
        register_lang!(configs, names, "lang-nginx", lang_nginx, "nginx");
        register_lang!(configs, names, "lang-ninja", lang_ninja, "ninja");
        register_lang!(configs, names, "lang-nix", lang_nix, "nix");
        register_lang!(configs, names, "lang-objc", lang_objc, "objc");
        register_lang!(configs, names, "lang-ocaml", lang_ocaml, "ocaml");
        register_lang!(configs, names, "lang-perl", lang_perl, "perl");
        register_lang!(configs, names, "lang-php", lang_php, "php");
        register_lang!(configs, names, "lang-powershell", lang_powershell, "powershell");
        register_lang!(configs, names, "lang-prolog", lang_prolog, "prolog");
        register_lang!(configs, names, "lang-python", lang_python, "python");
        register_lang!(configs, names, "lang-query", lang_query, "query");
        register_lang!(configs, names, "lang-r", lang_r, "r");
        register_lang!(configs, names, "lang-rescript", lang_rescript, "rescript");
        register_lang!(configs, names, "lang-ron", lang_ron, "ron");
        register_lang!(configs, names, "lang-ruby", lang_ruby, "ruby");
        register_lang!(configs, names, "lang-rust", lang_rust, "rust");
        register_lang!(configs, names, "lang-scala", lang_scala, "scala");
        register_lang!(configs, names, "lang-scheme", lang_scheme, "scheme");
        register_lang!(configs, names, "lang-scss", lang_scss, "scss");
        register_lang!(configs, names, "lang-sparql", lang_sparql, "sparql");
        register_lang!(configs, names, "lang-sql", lang_sql, "sql");
        register_lang!(configs, names, "lang-ssh-config", lang_ssh_config, "ssh-config");
        register_lang!(configs, names, "lang-starlark", lang_starlark, "starlark");
        register_lang!(configs, names, "lang-svelte", lang_svelte, "svelte");
        register_lang!(configs, names, "lang-swift", lang_swift, "swift");
        register_lang!(configs, names, "lang-textproto", lang_textproto, "textproto");
        register_lang!(configs, names, "lang-thrift", lang_thrift, "thrift");
        register_lang!(configs, names, "lang-tlaplus", lang_tlaplus, "tlaplus");
        register_lang!(configs, names, "lang-toml", lang_toml, "toml");
        register_lang!(configs, names, "lang-tsx", lang_tsx, "tsx");
        register_lang!(configs, names, "lang-typst", lang_typst, "typst");
        register_lang!(configs, names, "lang-uiua", lang_uiua, "uiua");
        register_lang!(configs, names, "lang-vb", lang_vb, "vb");
        register_lang!(configs, names, "lang-verilog", lang_verilog, "verilog");
        register_lang!(configs, names, "lang-vhdl", lang_vhdl, "vhdl");
        register_lang!(configs, names, "lang-vue", lang_vue, "vue");
        register_lang!(configs, names, "lang-x86asm", lang_x86asm, "x86asm");
        register_lang!(configs, names, "lang-xml", lang_xml, "xml");
        register_lang!(configs, names, "lang-yaml", lang_yaml, "yaml");
        register_lang!(configs, names, "lang-yuri", lang_yuri, "yuri");
        register_lang!(configs, names, "lang-zig", lang_zig, "zig");

        Self {
            configs,
            ts_highlighter: TsHighlighter::new(),
        }
    }

    /// Get a reference to a language configuration by name.
    pub fn get_config(&self, language: &str) -> Option<&HighlightConfiguration> {
        // Normalize language name
        let normalized = Self::normalize_language(language);
        self.configs.get(normalized)
    }

    /// Normalize language name to canonical form
    fn normalize_language(language: &str) -> &str {
        match language {
            "js" | "jsx" | "mjs" | "cjs" => "javascript",
            "ts" | "mts" | "cts" => "typescript",
            "py" | "py3" | "python3" => "python",
            "rb" => "ruby",
            "rs" => "rust",
            "sh" | "shell" => "bash",
            "yml" => "yaml",
            "htm" => "html",
            "cs" | "csharp" => "c-sharp",
            "c++" | "cxx" | "hpp" => "cpp",
            "golang" => "go",
            "hs" => "haskell",
            "ex" | "exs" => "elixir",
            "erl" => "erlang",
            "kt" | "kts" => "kotlin",
            "ml" => "ocaml",
            "pl" | "pm" => "perl",
            "ps1" | "pwsh" => "powershell",
            "sass" => "scss",
            "tf" | "terraform" => "hcl",
            "bat" | "cmd" => "batch",
            "dockerfile" | "docker" => "dockerfile",
            "h" => "c",
            "lisp" | "cl" => "commonlisp",
            "el" | "emacs-lisp" => "elisp",
            "jl" => "julia",
            "m" => "matlab",
            "mm" | "objective-c" => "objc",
            "json" | "jsonc" => "json",
            "scm" => "query",
            "rlang" => "r",
            "res" => "rescript",
            "rq" => "sparql",
            "mysql" | "postgresql" | "postgres" | "sqlite" => "sql",
            "pbtxt" | "textpb" => "textproto",
            "tla" => "tlaplus",
            "typ" => "typst",
            "ua" => "uiua",
            "vbnet" | "visualbasic" => "vb",
            "v" | "sv" | "systemverilog" => "verilog",
            "vhd" => "vhdl",
            "nasm" | "x86" => "x86asm",
            "xsl" | "xslt" | "svg" => "xml",
            "jinja" | "j2" => "jinja2",
            "gql" => "graphql",
            "vert" | "frag" => "glsl",
            "conf" | "cfg" => "ini",
            "bzl" | "bazel" => "starlark",
            "patch" => "diff",
            "dlang" => "d",
            "f#" | "fs" => "fsharp",
            other => other,
        }
    }

    /// Check if a language is supported
    pub fn is_supported(&self, language: &str) -> bool {
        self.get_config(language).is_some()
    }

    /// Get list of supported languages
    pub fn supported_languages(&self) -> Vec<&'static str> {
        self.configs.keys().copied().collect()
    }

    /// Highlight source code and return HTML string.
    ///
    /// This is the main entry point for highlighting. It automatically handles
    /// language injections (e.g., CSS/JS in HTML).
    pub fn highlight_to_html(&mut self, language: &str, source: &str) -> Result<String, HighlightError> {
        let mut output = Vec::new();
        self.highlight_to_html_writer(&mut output, language, source)?;
        String::from_utf8(output).map_err(|e| HighlightError::HighlightError(e.to_string()))
    }

    /// Highlight source code and write HTML to a writer.
    pub fn highlight_to_html_writer<W: Write>(
        &mut self,
        writer: &mut W,
        language: &str,
        source: &str,
    ) -> Result<(), HighlightError> {
        let normalized = Self::normalize_language(language);
        let config = self.configs.get(normalized)
            .ok_or_else(|| HighlightError::UnsupportedLanguage(language.to_string()))?;

        let highlights = self.ts_highlighter
            .highlight(config, source.as_bytes(), None, |lang| self.configs.get(lang))
            .map_err(|e| HighlightError::HighlightError(e.to_string()))?;

        let mut tag_stack: Vec<&'static str> = Vec::new();

        for event in highlights {
            let event = event.map_err(|e| HighlightError::HighlightError(e.to_string()))?;
            match event {
                HighlightEvent::Source { start, end } => {
                    html::write_escaped(writer, &source[start..end])?;
                }
                HighlightEvent::HighlightStart(Highlight(i)) => {
                    if let Some(tag) = highlights::tag(i) {
                        write!(writer, "<a-{tag}>")?;
                        tag_stack.push(tag);
                    }
                }
                HighlightEvent::HighlightEnd => {
                    if let Some(tag) = tag_stack.pop() {
                        write!(writer, "</a-{tag}>")?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "lang-rust")]
    fn test_rust_highlighting() {
        let mut highlighter = Highlighter::new();
        let html = highlighter.highlight_to_html("rust", "fn main() {}").unwrap();
        assert!(html.contains("<a-k>fn</a-k>")); // keyword
        assert!(html.contains("<a-f>main</a-f>")); // function
    }

    #[test]
    #[cfg(feature = "lang-html")]
    fn test_html_css_injection() {
        let mut highlighter = Highlighter::new();
        let html = highlighter
            .highlight_to_html("html", r#"<style>h1 { color: red; }</style>"#)
            .unwrap();
        assert!(
            html.contains("color"),
            "CSS property 'color' should be in output"
        );
    }

    #[test]
    #[cfg(feature = "lang-html")]
    fn test_html_js_injection() {
        let mut highlighter = Highlighter::new();
        let html = highlighter
            .highlight_to_html("html", r#"<script>let x = 1;</script>"#)
            .unwrap();
        assert!(
            html.contains("<a-k>let</a-k>"),
            "JS keyword 'let' should be highlighted"
        );
    }

    // Note: Svelte and Vue tests are in svelte_tests.rs and injection_tests.rs
}
