import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ParameterListSTranspiler} from "../expressions";

export class RaiseEventTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
// todo: event names must be fully qualified
    const eventName = node.findFirstExpression(abaplint.Expressions.EventName)?.concatTokens().toUpperCase();

    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    let extra = "";
    if (parameters) {
      extra = "," + new ParameterListSTranspiler().transpile(parameters, traversal).getCode();
    }

    return new Chunk().append(`abap.statements.raiseEvent("${eventName}"${extra});`, node, traversal);
  }

}