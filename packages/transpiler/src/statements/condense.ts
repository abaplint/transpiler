import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondenseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    const noGaps = node.concatTokens().search(/NO-GAPS.$/) > -1;
    return new Chunk("abap.statements.condense(" + target + ", {nogaps: " + noGaps + "});");
  }

}