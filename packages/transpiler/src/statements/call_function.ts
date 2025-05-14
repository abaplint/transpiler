import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SourceTranspiler} from "../expressions";
import {CallTranspiler} from "./call";

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

    const exceptions = node.findFirstExpression(abaplint.Expressions.ParameterListExceptions);
    if (exceptions) {
      ret.appendString("try {\n");
    }

    const calling = node.findExpressionAfterToken("CALLING");
    const dest = node.findDirectExpression(abaplint.Expressions.Destination)?.findDirectExpression(abaplint.Expressions.Source);
    if (dest) {
      const s = new SourceTranspiler(true).transpile(dest, traversal);
      param = param.replace("{", ",").replace(/}$/, "");
      ret.appendString(`await abap.statements.callFunction({name:${fmname},destination:${s.getCode()}${param}});`);
    } else if (calling) {
      param = param.replace("{", ",").replace(/}$/, "");
      // typically used in combination with STARTING NEW TASK so dont await,
      ret.appendString(`abap.statements.callFunction({name:${fmname},calling:this.${
        calling.concatTokens().toLowerCase()}${param}});`);
    } else {
      const illegalFunc = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_FUNC'", node.getFirstToken(), true);
      const call = `abap.FunctionModules[${fmname}]`;
      // eslint-disable-next-line max-len
      ret.appendString(`if (${call} === undefined) { if (${illegalFunc} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_FUNC not found"; } else { throw new ${illegalFunc}();} }\n`);
      ret.appendString(`await ${call}(${param});`);
    }

    if (exceptions) {
      const build = CallTranspiler.buildExceptions(exceptions);
      ret.appendString(build.post);
    }

    return ret;
  }

}