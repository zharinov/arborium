; Starlark highlights query (simplified from Python)

; Comments
(comment) @comment

; Strings
(string) @string

; Numbers
(integer) @number
(float) @number

; Boolean and none literals
(true) @constant.builtin
(false) @constant.builtin
(none) @constant.builtin

; Function definitions
(function_definition
  name: (identifier) @function.definition)

; Function calls
(call
  function: (identifier) @function.call)

(call
  function: (attribute
    attribute: (identifier) @function.call))

; Parameters
(parameters
  (identifier) @variable.parameter)

(default_parameter
  name: (identifier) @variable.parameter)

(lambda_parameters
  (identifier) @variable.parameter)

; Keyword arguments
(keyword_argument
  name: (identifier) @variable.parameter)

; Attributes
(attribute
  attribute: (identifier) @property)

; Load statement
(load_statement) @keyword.import
(aliased_load
  (identifier) @variable)

; Decorators
(decorator
  "@" @punctuation.special
  (identifier) @attribute)

; Keywords
[
  "def"
  "lambda"
] @keyword.function

[
  "if"
  "elif"
  "else"
] @keyword.conditional

[
  "for"
  "in"
  "break"
  "continue"
] @keyword.repeat

[
  "return"
  "pass"
  "load"
] @keyword

[
  "and"
  "or"
  "not"
] @keyword.operator

; Operators
[
  "+"
  "-"
  "*"
  "**"
  "/"
  "//"
  "%"
  "|"
  "&"
  "^"
  "~"
  "<"
  ">"
  "<="
  ">="
  "=="
  "!="
  "="
  "+="
  "-="
  "*="
  "/="
  "//="
  "%="
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
  ","
  "."
  ":"
] @punctuation.delimiter

; Identifiers (fallback)
(identifier) @variable
