import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let extra = "";
    const newLine = node.findDirectTokenByText("/") !== undefined;
    if (newLine === true) {
      extra = ", {newLine: true}";
    }
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    if (source.startsWith("'@KERNEL ")) {
      return source.substr(9, source.length - 10);
    } else {
      return "abap.statements.write(" + source + extra + ");";
    }
  }

}