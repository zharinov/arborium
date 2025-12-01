; VB.NET highlights query

; Comments
(comment) @comment

; Strings
(string_literal) @string
(interpolated_string_literal) @string
(character_literal) @character

; Numbers
(integer_literal) @number
(floating_point_literal) @number

; Boolean
(boolean_literal) @constant.builtin

; Date literals
(date_literal) @string.special

; Type declarations
(class_block
  (identifier) @type.definition)

(module_block
  (identifier) @type.definition)

(structure_block
  (identifier) @type.definition)

(interface_block
  (identifier) @type.definition)

(enum_block
  (identifier) @type.definition)

; Method declarations
(method_declaration
  (identifier) @function.definition)

; Property declarations
(property_declaration
  (identifier) @property)

; Parameter list
(parameter
  (identifier) @variable.parameter)

; Type references
(primitive_type) @type.builtin

; Member access
(member_access
  (identifier) @property)

; Invocations
(invocation
  (identifier) @function.call)

; Identifiers
(identifier) @variable

; Enum members
(enum_member
  (identifier) @constant)

; Modifiers
(modifier) @keyword.modifier

; Attributes
(attribute) @attribute

; Keywords - use a simple list of common VB.NET keywords
[
  "Class"
  "Module"
  "Namespace"
  "Structure"
  "Interface"
  "Enum"
  "Sub"
  "Function"
  "Property"
  "Get"
  "Set"
  "End"
  "New"
  "Dim"
  "Const"
  "As"
  "Of"
  "Inherits"
  "Implements"
  "Imports"
  "Event"
  "Private"
  "Protected"
  "Public"
  "Friend"
  "Shared"
  "Static"
  "ReadOnly"
  "ByVal"
  "ByRef"
  "Optional"
  "Me"
  "MyBase"
  "Nothing"
  "If"
  "Then"
  "Else"
  "ElseIf"
  "Select"
  "Case"
  "For"
  "Each"
  "To"
  "Step"
  "Next"
  "While"
  "Do"
  "Loop"
  "Until"
  "Continue"
  "Exit"
  "Try"
  "Catch"
  "Finally"
  "Throw"
  "Return"
  "GoTo"
  "True"
  "False"
  "And"
  "Or"
  "Not"
  "Xor"
  "Is"
  "IsNot"
  "Like"
  "TypeOf"
  "With"
  "Using"
  "SyncLock"
  "ReDim"
  "AddHandler"
  "RemoveHandler"
  "RaiseEvent"
  "Overrides"
  "Overloads"
  "Overridable"
  "MustOverride"
  "MustInherit"
  "NotInheritable"
  "Partial"
  "Shadows"
  "WithEvents"
  "Delegate"
] @keyword

; Operators
[
  "="
  "<>"
  "<"
  ">"
  "<="
  ">="
  "+"
  "-"
  "*"
  "/"
  "\\"
  "^"
  "&"
  "+="
  "-="
  "*="
  "/="
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
] @punctuation.delimiter
