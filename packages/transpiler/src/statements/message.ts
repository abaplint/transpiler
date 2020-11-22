import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class MessageTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    const into = node.findExpressionAfterToken("INTO");
    if (into) {
      options.push("into: " + traversal.traverse(into));
    }

    // todo

    return "abap.statements.message({" + options.join(", ") + "});";
  }

}