import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ClassImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)!.getFirstToken();

    const def = traversal.getClassDefinition(token);

    let ret = "class " + token.getStr().toLowerCase();

    if (token.getStr().toLowerCase() === "cx_root") {
      // special case for exceptions
      ret += " extends Error";
    } else if (def?.getSuperClass()) {
      ret += " extends " + def?.getSuperClass()?.toLowerCase();
    }

    return ret + " {";
  }

}