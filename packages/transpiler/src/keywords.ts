// https://www.w3schools.com/js/js_reserved.asp
export const DEFAULT_KEYWORDS = new Set<string>([
  "abstract",	"arguments", "await",
  "break",	"byte", "catch",
//  "char",
  "class", "const", "continue",
  "debugger",	"default", "do",
  "double",	"else", "enum", "eval",
  "export",	"extends", "false", "final",
  "finally", "for", "function",
  "goto",	"if", "implements", "import",
  "in",	"instanceof", "interface",
  "let",	"long", "native", "new",
  "null",	"package", "private", // "protected",
  "public",	"return", "short", "static",
  "switch", "synchronized", "this",
  "throw",	"throws", "transient", "true",
  "try",	"typeof", "var", "void",
  "delete",
  "volatile",	"while", "yield",
// both produce a SyntaxError in the emitted module: they are reserved words
// in JavaScript and legal ABAP names. "super" is NOT in this list on purpose -
// the emitted code uses the JS `super` for ABAP's `super->method( )`, and
// escaping it turns that call into `$super.get().method()`
  "case", "with"]);
