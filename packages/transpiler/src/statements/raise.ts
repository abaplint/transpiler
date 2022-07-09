import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SourceTranspiler} from "../expressions";

export class RaiseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const classNameToken = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    const className = classNameToken?.getStr();
    if (className === undefined) {
      const s = node.findFirstExpression(abaplint.Expressions.SimpleSource2);
      if (s === undefined) {
        const name = node.findFirstExpression(abaplint.Expressions.ExceptionName)?.concatTokens().toLowerCase();
        return new Chunk().append(`throw new Error({classic: "${name}"});`, node, traversal);
      }
      const sCode = new SourceTranspiler(true).transpile(s, traversal).getCode();
      return new Chunk().append(`throw ${sCode};`, node, traversal);
    }

    let p = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      p = traversal.traverse(parameters).getCode();
    }

    const look = traversal.lookupClassOrInterface(classNameToken?.getStr(), classNameToken);

    return new Chunk().append(`throw await (new ${look}()).constructor_(${p});`, node, traversal);
  }

}