import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ClassImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)!.getFirstToken();

    let extra = "";
    const definition = traversal.getClassDefinition(token);
    if (definition && definition.isGlobal()) {
      extra = "export ";
    }

    return extra + "class " + token.getStr() + " {";
  }

}