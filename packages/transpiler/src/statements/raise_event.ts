import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ParameterListSTranspiler} from "../expressions";

export class RaiseEventTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const eventName = Traversal.escapeNamespace(node.findFirstExpression(abaplint.Expressions.EventName)?.concatTokens())?.toLowerCase().replace("~", "$");

    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    let extra = "";
    if (parameters) {
      extra = "," + new ParameterListSTranspiler().transpile(parameters, traversal).getCode();
    }

    return new Chunk().append(`await abap.statements.raiseEvent(this.${eventName}, this.me${extra});`, node, traversal);
  }

}