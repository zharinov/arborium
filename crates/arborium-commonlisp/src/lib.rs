//! COMMONLISP grammar for tree-sitter
//!
//! This crate provides the commonlisp language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

unsafe extern "C" {
    fn tree_sitter_commonlisp() -> Language;
}

/// Returns the commonlisp tree-sitter language.
pub fn language() -> Language {
    unsafe { tree_sitter_commonlisp() }
}

/// The highlight query for commonlisp.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../../grammars/tree-sitter-commonlisp/queries/highlights.scm");

/// The injections query for commonlisp (empty - no injections available).
pub const INJECTIONS_QUERY: &str = "";

/// The locals query for commonlisp (empty - no locals available).
pub const LOCALS_QUERY: &str = "";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language() {
        let lang = language();
        assert!(lang.version() > 0);
    }
}
