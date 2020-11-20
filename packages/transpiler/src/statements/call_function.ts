import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CallFunctionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, _traversal: Traversal): string {
    const fmname = node.findDirectExpression(abaplint.Expressions.FunctionName)?.concatTokens();
    if (fmname === undefined) {
      throw "CallFunctionTranspilerNameNotFound";
    }

    // todo, handle parameters
    let ret = "";

    ret += `abap.FunctionModules[${fmname}]();\n`;
    ret += `throw "CallFunctionTranspilerTodo";`;

    return ret;
  }

}