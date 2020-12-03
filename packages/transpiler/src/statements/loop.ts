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

    const unique1 = UniqueIdentifier.get();
    let target = "";
    const into = node.findDirectExpression(abaplint.Expressions.Target);
    if (into) {
      target = traversal.traverse(into) + ".set(" + unique1 + ");";
    } else {
      const assigning = node.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (assigning) {
        target = traversal.traverse(assigning) + " = " + unique1 + ";";
      }
    }

    const unique2 = UniqueIdentifier.get();
    return "let " + unique2 + " = 1\n" +
      "for (let " + unique1 + " of " + source + ".array()) {\n" +
      "abap.builtin.sy.get().tabix.set(" + unique2 + "++);\n" +
      target;
  }

}