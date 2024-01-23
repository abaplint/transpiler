import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class UnpackTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const s = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Source));
    const t = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target));
    return new Chunk(`await abap.statements.unpack(${s.getCode()},${t.getCode()});`);
  }

}