import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class GetLocaleTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const t = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target));
    return new Chunk(`abap.statements.getLocale(${t.getCode()});`);
  }

}