import * as abaplint from "@abaplint/core";

/** Replaces javascript keywords in ABAP source code, in-memory only */
export class Keywords {

  public handle(reg: abaplint.IRegistry) {
    reg.parse();

    for (const o of reg.getObjects()) {
      if (!(o instanceof abaplint.ABAPObject)) {
        continue;
      }
      for (const f of o.getABAPFiles()) {
        let tokens: abaplint.Token[] = [];
        for (const s of f.getStatements()) {
          tokens = tokens.concat(this.traverse(s, f));
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

  private traverse(node: abaplint.INode, file: abaplint.ABAPFile): abaplint.Token[] {
// https://www.w3schools.com/js/js_reserved.asp
    const keywords: string[] = [
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
      "null",	"package", "private", "protected",
      "public",	"return", "short", "static",
      "switch", "synchronized", "this",
      "throw",	"throws", "transient", "true",
      "try",	"typeof", "var", "void",
      "volatile",	"while", "yield"];
// "with"
// "delete"

    let ret: abaplint.Token[] = [];
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNodeRegex) {
        if (keywords.some(k => k === c.getFirstToken().getStr().toLowerCase())) {
          ret.push(c.getFirstToken());
        }
      } else if (c instanceof abaplint.Nodes.TokenNode) {
        continue;
      } else {
        ret = ret.concat(this.traverse(c, file));
      }
    }

    return ret;
  }

}