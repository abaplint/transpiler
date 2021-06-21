import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let extra = "";
    let source = "";
    const newLine = node.findFirstExpression(abaplint.Expressions.WriteOffsetLength)?.findDirectTokenByText("/") !== undefined;

    const expr = node.findDirectExpression(abaplint.Expressions.Source);
    if (expr === undefined) {
      source = "''";
      extra = ", {newLine: true,skipLine: true}";
    } else {
      if (newLine === true) {
        extra = ", {newLine: true}";
      }
      const concat = expr.concatTokens();
      if (concat.startsWith("'@KERNEL ")) {
        // @KERNEL commands must be taken verbatim
        return concat.substr(9, concat.length - 10);
      }
      source = new SourceTranspiler().transpile(expr, traversal);
    }
    return "abap.statements.write(" + source + extra + ");";
  }

}