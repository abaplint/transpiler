import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FreeMemoryTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const id = node.findDirectExpression(abaplint.Expressions.Source);
    return new Chunk(`abap.statements.freeMemory({${id === undefined ? "" : "id: " + traversal.traverse(id).getCode()}});`);
  }

}