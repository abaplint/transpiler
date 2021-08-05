import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallFunctionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const fmname = node.findDirectExpression(abaplint.Expressions.FunctionName)?.concatTokens();
    if (fmname === undefined) {
      throw "CallFunctionTranspilerNameNotFound";
    }

    let param = "";
    const fmp = node.findDirectExpression(abaplint.Expressions.FunctionParameters);
    if (fmp) {
      param = traversal.traverse(fmp).getCode();
    }

    const ret = `abap.FunctionModules[${fmname}](${param});`;

    return new Chunk(ret);
  }

}