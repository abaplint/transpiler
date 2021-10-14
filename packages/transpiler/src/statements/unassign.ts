import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class UnassignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const fs = new FieldSymbolTranspiler().transpile(
      node.findDirectExpression(abaplint.Expressions.TargetFieldSymbol)!, traversal).getCode();
    return new Chunk().append(`${fs}.unassign();`, node.getLastToken(), traversal);
  }

}