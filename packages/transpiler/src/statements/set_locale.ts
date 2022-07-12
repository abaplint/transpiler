import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SetLocaleTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const s = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Source));
    return new Chunk(`abap.statements.setLocale(${s.getCode()});`);
  }

}