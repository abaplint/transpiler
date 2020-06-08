import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {UniqueIdentifier} from "../unique_identifier";
import {Traversal} from "../traversal";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    if (!(node.get() instanceof abaplint.Statements.Loop)) {
      throw new Error("LoopTranspiler, unexpected node");
    }

    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.BasicSource));
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const unique = UniqueIdentifier.get();
    return "for (let " + unique + " of " + source + ".array()) {\n" +
      target + ".set(" + unique + ");";
  }

}