import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let extra = "";
    const newLine = node.findDirectTokenByText("/") !== undefined;
    const expr = node.findDirectExpression(abaplint.Expressions.Source);

    let concat = "";
    let source = "";
    if (expr === undefined) {
      if (newLine === true) {
        extra = "'', {newLine: true, skipLine: true}";
      } else {
        throw new Error("WriteTranspiler, no source expression found");
      }

    } else {
      if (newLine === true) {
        extra = ", {newLine: true}";
      }
      concat = expr.concatTokens();
      if (concat.startsWith("'@KERNEL ")) {
        // @KERNEL commands must be taken verbatim
        return concat.substr(9, concat.length - 10);
      }
      source = new SourceTranspiler().transpile(expr, traversal);
    }
    return "abap.statements.write(" + source + extra + ");";
  }

}