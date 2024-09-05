import * as abaplint from "@abaplint/core";

// https://www.w3schools.com/js/js_reserved.asp
export const DEFAULT_KEYWORDS: string[] = [
  "abstract",	"arguments", "await",
  "break",	"byte", "catch",
  "char",	"class", "const", "continue",
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
  "volatile",	"while", "yield"];
// "with"
// "delete"

/** Replaces javascript keywords in ABAP source code, in-memory only */
export class Keywords {
  private readonly keywords: string[] = [];

  public constructor(keywords?: string[]) {
    if (keywords !== undefined) {
      this.keywords = keywords;
    } else {
      this.keywords = DEFAULT_KEYWORDS;
    }
  }

  public handle(reg: abaplint.IRegistry) {
    if (this.keywords.length === 0) {
      return;
    }
    reg.parse();

    for (const o of reg.getObjects()) {
      if (!(o instanceof abaplint.ABAPObject)) {
        continue;
      }
      for (const f of o.getABAPFiles()) {
        const tokens: abaplint.Token[] = [];
        for (const s of f.getStatements()) {
          if (s.get() instanceof abaplint.MacroCall) {
            tokens.push(...this.handleMacro(s));
          } else {
            tokens.push(...this.traverse(s, f));
          }
        }
        if (tokens.length === 0) {
          continue;
        }
        const rows = f.getRawRows();
        for (const t of tokens.reverse()) {
          const original = rows[t.getRow() - 1];
          const index = t.getEnd().getCol() - 1;
          rows[t.getRow() - 1] = original.substring(0, index) + "_" + original.substring(index);
        }
        reg.updateFile(new abaplint.MemoryFile(f.getFilename(), rows.join("\n")));
      }
    }

    reg.parse();
  }

  private handleMacro(node: abaplint.Nodes.StatementNode): abaplint.Token[] {
    const tokens: abaplint.Token[] = [];

    for (const token of node.getTokens()) {
      for (const k of this.keywords) {
        const lower = token.getStr().toLowerCase();
        if (k === lower
            || "!" + k === lower
            || lower.endsWith("~" + k)) {
          tokens.push(token);
        }
      }
    }

    return tokens;
  }

  private traverse(node: abaplint.INode, file: abaplint.ABAPFile): abaplint.Token[] {

    const ret: abaplint.Token[] = [];
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNodeRegex) {
        const token = c.getFirstToken();

        const start = token.getStart();
        if (start instanceof abaplint.VirtualPosition) {
          continue;
        }
        for (const k of this.keywords) {
          const lower = token.getStr().toLowerCase();
          if (k === lower
              || "!" + k === lower
              || lower.endsWith("~" + k)) {
            ret.push(token);
            break;
          }
        }
      } else if (c instanceof abaplint.Nodes.TokenNode) {
        continue;
      } else {
        ret.push(...this.traverse(c, file));
      }
    }

    return ret;
  }

}