//! MARKDOWN-INLINE grammar for tree-sitter
//!
//! This crate provides the markdown-inline language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

unsafe extern "C" {
    fn tree_sitter_markdown_inline() -> Language;
}

/// Returns the markdown-inline tree-sitter language.
pub fn language() -> Language {
    unsafe { tree_sitter_markdown_inline() }
}

/// The highlight query for markdown-inline.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../queries/highlights.scm");

/// The injections query for markdown-inline.
pub const INJECTIONS_QUERY: &str = include_str!("../queries/injections.scm");

/// The locals query for markdown-inline (empty - no locals available).
pub const LOCALS_QUERY: &str = "";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grammar() {
        arborium_test_harness::test_grammar(
            language(),
            "markdown-inline",
            HIGHLIGHTS_QUERY,
            INJECTIONS_QUERY,
            LOCALS_QUERY,
            env!("CARGO_MANIFEST_DIR"),
        );
    }
}
