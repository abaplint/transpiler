import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ReceiveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const fmchild = node.findDirectExpression(abaplint.Expressions.FunctionName)?.getFirstChild();
    if (fmchild === undefined) {
      throw new Error("ReceiveTranspilerNameNotFound");
    }
    const fmname = fmchild.concatTokens().toUpperCase();

    let param = "";
    const fmp = node.findDirectExpression(abaplint.Expressions.ReceiveParameters);
    if (fmp) {
      param = traversal.traverse(fmp).getCode();
    }

    const ret = new Chunk();
    ret.appendString(`await abap.statements.receive({name:${fmname},${param}});`);
    return ret;
  }

}