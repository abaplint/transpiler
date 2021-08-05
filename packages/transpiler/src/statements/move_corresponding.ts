import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MoveCorrespondingTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source)).getCode();

    const ret = `abap.statements.moveCorresponding(${source}, ${target});`;

    return new Chunk(ret);
  }

}