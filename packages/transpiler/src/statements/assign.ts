import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AssignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const source = new SourceTranspiler(false).transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    const fs = new FieldSymbolTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.FieldSymbol)!, traversal);

    const options: string[] = [];
    options.push("target: " + fs);
    options.push("source: " + source);

    return "abap.statements.assign({" + options.join(", ") + "});";
  }

}