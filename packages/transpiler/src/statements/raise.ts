import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class RaiseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const classNameToken = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    const className = classNameToken?.getStr();
    if (className === undefined) {
      throw "RaiseTranspilerNameNotFound";
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