import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {SourceTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class ModifyInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

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

    const whereNode = node.findDirectExpression(abaplint.Expressions.ComponentCond);
    if (whereNode) {
      // todo, replacing "await" is a hack
      extra.push("where: " + traversal.traverse(whereNode).getCode().replace("await ", ""));
    }

    const transporting = node.findDirectExpressions(abaplint.Expressions.ComponentChainSimple);
    if (transporting.length > 0) {
      const list: string[] = [];
      for (const t of transporting) {
        list.push("\"" + t.concatTokens().toLowerCase() + "\"");
      }
      extra.push("transporting: [" + list.join(",") + "]");
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return new Chunk()
      .append("abap.statements.modifyInternal(", node, traversal)
      .appendString(target + concat)
      .append(");", node.getLastToken(), traversal);
  }

}