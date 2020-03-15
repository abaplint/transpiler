import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);
    return target + ".set(" + source + ");";
  }

}