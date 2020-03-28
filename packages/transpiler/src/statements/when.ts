import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class WhenTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "OTHERS") {
        ret = ret + "default:";
      } else if (c instanceof abaplint.Nodes.ExpressionNode && c.get() instanceof abaplint.Expressions.Source) {
        const source = new SourceTranspiler(true).transpile(c, traversal);
        ret = "case " + source + ":";
      } else if (c instanceof abaplint.Nodes.ExpressionNode && c.get() instanceof abaplint.Expressions.Or) {
        const source = new SourceTranspiler(true).transpile(c.findDirectExpression(abaplint.Expressions.Source)!, traversal);
        ret = ret + "\ncase " + source + ":";
      }
    }

    return ret;
  }

}