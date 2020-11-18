import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class RaiseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const className = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken().getStr();
    if (className === undefined) {
      throw "RaiseTranspilerNameNotFound";
    }

    let p = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      p = traversal.traverse(parameters);
    }

    return `throw new ${className}(${p});`;
  }

}