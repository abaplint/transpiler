import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {UniqueIdentifier} from "../unique_identifier";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    if (!(node.get() instanceof abaplint.Statements.Loop)) {
      throw new Error("LoopTranspiler, unexpected node");
    }

    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.BasicSource));

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

    const whereNode = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    const where = whereNode ? ", " + traversal.traverse(whereNode) : "";

    return `for (const ${unique1} of abap.statements.loop(${source}${where})) {\n${target}`;
  }

}