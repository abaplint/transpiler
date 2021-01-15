import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {UniqueIdentifier} from "../unique_identifier";
import {SourceTranspiler} from "../expressions";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    if (!(node.get() instanceof abaplint.Statements.Loop)) {
      throw new Error("LoopTranspiler, unexpected node");
    }

    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SimpleSource2));

    const unique1 = UniqueIdentifier.get();
    let target = "";
    const into = node.findDirectExpression(abaplint.Expressions.Target);
    if (into) {
      target = traversal.traverse(into) + ".set(" + unique1 + ");";
    } else {
      const assigning = node.findFirstExpression(abaplint.Expressions.FSTarget)?.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (assigning) {
        target = traversal.traverse(assigning) + ".assign(" + unique1 + ");";
      }
    }

    const extra: string[] = [];
    const fromNode = node.findExpressionAfterToken("FROM");
    if (fromNode) {
      const from = new SourceTranspiler().transpile(fromNode, traversal);
      extra.push("from: " + from);
    }

    const toNode = node.findExpressionAfterToken("TO");
    if (toNode) {
      const to = new SourceTranspiler().transpile(toNode, traversal);
      extra.push("to: " + to);
    }

    const whereNode = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    if (whereNode) {
      const where = traversal.traverse(whereNode);
      extra.push("where: " + where);
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }
    return `for (const ${unique1} of abap.statements.loop(${source}${concat})) {\n${target}`;
  }

}