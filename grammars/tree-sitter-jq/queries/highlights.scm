; jq highlights query

; Comments
(comment) @comment

; Strings
(string) @string
(string_interp) @string
(format) @string.special

; Numbers
(number) @number

; Keywords
(keyword) @keyword

; Built-in keywords
[
  "if"
  "then"
  "else"
  "elif"
  "end"
  "as"
  "def"
  "reduce"
  "foreach"
  "try"
  "catch"
  "and"
  "or"
  "not"
  "import"
  "include"
  "module"
] @keyword

; Function definitions
(funcdef
  (funcname) @function.definition)

; Function names in calls
(funcname) @function.call

; Variables
(variable) @variable

; Identifiers (field access)
(identifier) @property

; Object keys
(objectkey) @property

; Index access
(index) @punctuation.bracket

; Recursion operator
(recurse) @operator

; Module/import
(import_) @keyword.import
(moduleheader) @keyword.import

; Binding (as pattern)
(binding) @variable

; Patterns
(objectpattern) @variable
(objectpatterns) @variable
(arraypatterns) @variable

; Operators
[
  "|"
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "//"
  "="
  "|="
  "+="
  "-="
  "*="
  "/="
  "%="
  "//="
  "?"
  "."
  ".."
] @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ":"
  ";"
  ","
] @punctuation.delimiter

; Special values
[
  "null"
  "true"
  "false"
] @constant.builtin
