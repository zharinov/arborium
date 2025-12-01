//! HCL grammar for tree-sitter
//!
//! This crate provides the hcl language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

unsafe extern "C" {
    fn tree_sitter_hcl() -> Language;
}

/// Returns the hcl tree-sitter language.
pub fn language() -> Language {
    unsafe { tree_sitter_hcl() }
}

/// The highlight query for hcl.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../../grammars/tree-sitter-hcl/queries/highlights.scm");

/// The injections query for hcl (empty - no injections available).
pub const INJECTIONS_QUERY: &str = "";

/// The locals query for hcl (empty - no locals available).
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
