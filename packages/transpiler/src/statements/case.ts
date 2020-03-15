import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class CaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const source = new SourceTranspiler(true).transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    return "switch (" + source + ") {";
  }

}