import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class GetLocaleTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const t = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target));
    return new Chunk(`abap.statements.getLocale(${t.getCode()});`);
  }

}