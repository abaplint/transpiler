import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Chunk} from "../chunk";

export class ReadTableTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    let prefix = "";
    const s = node.findDirectExpression(abaplint.Expressions.SimpleSource2);
    const ret = traversal.traverse(s).getCode();

    const extra: string[] = [];
    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler().transpile(index, traversal).getCode();
      extra.push("index: " + s);
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from) {
      const s = new SourceTranspiler().transpile(from, traversal).getCode();
      extra.push("from: " + s);
    }

    const rt = node.findDirectExpression(abaplint.Expressions.ReadTableTarget);
    const target = rt?.findDirectExpression(abaplint.Expressions.Target);
    const fs = rt?.findDirectExpression(abaplint.Expressions.FSTarget);
    if (rt && fs) {
      const name = new FieldSymbolTranspiler().transpile(fs, traversal).getCode();
      extra.push("assigning: " + name);
    } else if (target) {
      const name = traversal.traverse(target).getCode();
      if (rt?.findDirectTokenByText("REFERENCE")) {
        extra.push("referenceInto: " + name);
      } else {
        extra.push("into: " + name);
      }
    }

    const compare = node.findDirectExpression(abaplint.Expressions.ComponentCompareSimple);
    if (compare) {
      const conds: string[] = [];
      const count = compare.getChildren().length / 3;
      for (let i = 0; i < count; i++) {
        const left = compare.getChildren()[i * 3];
        const source = compare.getChildren()[(i * 3) + 2];

        const s = traversal.traverse(source).getCode();

        let field = left.concatTokens().toLowerCase();
        while(field.includes("->")) {
          field = field.replace("->", ".get().");
        }
        while(field.includes("-")) {
          field = field.replace("-", ".get().");
        }
        field = Traversal.escapeNamespace(field)!.replace("~", "$");

        if (left.get() instanceof abaplint.Expressions.Dynamic
            && left instanceof abaplint.Nodes.ExpressionNode) {
          const concat = left.concatTokens().toLowerCase();
          field = concat.substring(2, concat.length - 2);
        }
        if (s.includes("await")) {
          const id = UniqueIdentifier.get();
          prefix += "const " + id + " = " + s + ";\n";
          conds.push("abap.compare.eq(i." + field + ", " + id + ")");
        } else {
          conds.push("abap.compare.eq(i." + field + ", " + s + ")");
        }
      }
      extra.push("withKey: (i) => {return " + conds.join(" && ") + ";}");
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return new Chunk()
      .append(prefix + "abap.statements.readTable(", node, traversal)
      .appendString(ret + concat)
      .append(");", node.getLastToken(), traversal);
  }

}