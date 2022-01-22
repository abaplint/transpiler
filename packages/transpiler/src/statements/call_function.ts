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

    const ret = new Chunk();

    const dest = node.findDirectExpression(abaplint.Expressions.Destination)?.findDirectExpression(abaplint.Expressions.Source);
    if (dest) {
      param = param.replace("{", ",").replace(/}$/, "");
      ret.appendString(`abap.statements.callFunction({name:${fmname},destination:${dest.concatTokens()}${param}});`);
    } else {
      ret.appendString(`abap.FunctionModules[${fmname}](${param});`);
    }

    return ret;
  }

}