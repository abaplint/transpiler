import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SourceTranspiler} from "../expressions";

export class CallFunctionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const fmchild = node.findDirectExpression(abaplint.Expressions.FunctionName)?.getFirstChild();
    if (fmchild === undefined) {
      throw new Error("CallFunctionTranspilerNameNotFound");
    }
    let fmname = "";
    if (fmchild instanceof abaplint.Nodes.ExpressionNode
        && fmchild.get() instanceof abaplint.Expressions.FieldChain) {
      fmname = new FieldChainTranspiler(true).transpile(fmchild, traversal).getCode();
      fmname = fmname + ".trimEnd()";
    } else {
      fmname = fmchild.concatTokens().toUpperCase();
    }

    let param = "";
    const fmp = node.findDirectExpression(abaplint.Expressions.FunctionParameters);
    if (fmp) {
      param = traversal.traverse(fmp).getCode();
    }

    const ret = new Chunk();

    const dest = node.findDirectExpression(abaplint.Expressions.Destination)?.findDirectExpression(abaplint.Expressions.Source);
    if (dest) {
      const s = new SourceTranspiler(true).transpile(dest, traversal);
      param = param.replace("{", ",").replace(/}$/, "");
      ret.appendString(`await abap.statements.callFunction({name:${fmname},destination:${s.getCode()}${param}});`);
    } else {
      ret.appendString(`await abap.FunctionModules[${fmname}](${param});`);
    }

    return ret;
  }

}