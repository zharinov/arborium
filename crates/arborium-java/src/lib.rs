//! JAVA grammar for tree-sitter
//!
//! This crate provides the java language grammar for use with tree-sitter.

use tree_sitter_patched_arborium::Language;

unsafe extern "C" {
    fn tree_sitter_java() -> Language;
}

/// Returns the java tree-sitter language.
pub fn language() -> Language {
    unsafe { tree_sitter_java() }
}

/// The highlight query for java.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../../grammars/tree-sitter-java/queries/highlights.scm");

/// The injections query for java (empty - no injections available).
pub const INJECTIONS_QUERY: &str = "";

/// The locals query for java (empty - no locals available).
pub const LOCALS_QUERY: &str = "";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language() {
        let lang = language();
        assert!(lang.version() > 0);
    }

    #[test]
    fn test_parse_sample() {
        use tree_sitter_patched_arborium::Parser;

        let source = include_str!("../sample.java");

        let mut parser = Parser::new();
        parser.set_language(&language()).expect("Failed to set language");

        let tree = parser.parse(source, None).expect("Failed to parse");
        let root = tree.root_node();

        // Basic checks
        assert!(!root.has_error(), "Parse tree has errors");
        assert!(root.child_count() > 0, "Parse tree is empty");
    }

    #[test]
    fn test_highlight_query_valid() {
        use tree_sitter_patched_arborium::Query;

        // This will fail if the query has syntax errors or references invalid node types
        let _query = Query::new(&language(), HIGHLIGHTS_QUERY)
            .expect("Highlights query is invalid");
    }
}
