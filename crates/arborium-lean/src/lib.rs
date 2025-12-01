//! LEAN grammar for tree-sitter
//!
//! This crate provides the lean language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

unsafe extern "C" {
    fn tree_sitter_lean() -> Language;
}

/// Returns the lean tree-sitter language.
pub fn language() -> Language {
    unsafe { tree_sitter_lean() }
}

/// The highlight query for lean.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../../grammars/tree-sitter-lean/queries/highlights.scm");

/// The injections query for lean (empty - no injections available).
pub const INJECTIONS_QUERY: &str = "";

/// The locals query for lean (empty - no locals available).
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
