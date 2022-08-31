import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallFunctionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const fmname = node.findDirectExpression(abaplint.Expressions.FunctionName)?.concatTokens().toUpperCase();
    if (fmname === undefined) {
      throw new Error("CallFunctionTranspilerNameNotFound");
    }

    let param = "";
    const fmp = node.findDirectExpression(abaplint.Expressions.FunctionParameters);
    if (fmp) {
      param = traversal.traverse(fmp).getCode();
    }

    const ret = new Chunk();

    const dest = node.findDirectExpression(abaplint.Expressions.Destination)?.findDirectExpression(abaplint.Expressions.Source);
    if (dest) {
      param = param.replace("{", ",").replace(/}$/, "");
      ret.appendString(`await abap.statements.callFunction({name:${fmname},destination:${dest.concatTokens()}${param}});`);
    } else {
      ret.appendString(`await abap.FunctionModules[${fmname}](${param});`);
    }

    return ret;
  }

}