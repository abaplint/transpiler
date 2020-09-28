import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ClassImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, _traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)!.getFirstToken();

    return "class " + token.getStr().toLowerCase() + " {";
  }

}