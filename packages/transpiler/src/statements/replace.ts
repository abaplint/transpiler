import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ReplaceTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    const concat = node.concatTokens().toUpperCase();
    const all = concat.startsWith("REPLACE ALL");


    const extra: string[] = [];
    const w = node.findExpressionAfterToken("WITH");
    if (w) {
      extra.push("with: " + new SourceTranspiler().transpile(w, traversal).getCode());
    }
    const o = node.findExpressionAfterToken("OF");
    if (o && o.get() instanceof abaplint.Expressions.Source) {
      extra.push("of: " + new SourceTranspiler().transpile(o, traversal).getCode());
    }

    const r = node.findDirectExpression(abaplint.Expressions.FindType);
    const type = r?.concatTokens().toUpperCase();
    if (type === "REGEX") {
      extra.push("regex: " + new SourceTranspiler().transpile(sources[0], traversal).getCode());
    }

    if (o === undefined && o === undefined) {
      extra.push("of: " + new SourceTranspiler().transpile(sources[0], traversal).getCode());
    }

    return new Chunk()
      .append("abap.statements.replace({target:", node, traversal)
      .appendString(target + ", all:" + all + ", " + extra.join(","))
      .append("});", node.getLastToken(), traversal);
  }

}