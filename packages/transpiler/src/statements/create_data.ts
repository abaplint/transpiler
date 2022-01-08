import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CreateDataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const targetNode = node.findDirectExpression(abaplint.Expressions.Target);
    const target = traversal.traverse(targetNode);

    return new Chunk("abap.statements.createData(" + target.getCode() + ");");
  }

}