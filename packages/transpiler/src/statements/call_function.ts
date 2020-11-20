import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CallFunctionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const fmname = node.findDirectExpression(abaplint.Expressions.FunctionName)?.concatTokens();
    if (fmname === undefined) {
      throw "CallFunctionTranspilerNameNotFound";
    }

    let param = "";
    const fmp = node.findDirectExpression(abaplint.Expressions.FunctionParameters);
    if (fmp === undefined) {
      param = traversal.traverse(fmp);
    }

    const ret = `abap.FunctionModules[${fmname}](${param});\n`;

    return ret;
  }

}