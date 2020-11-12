import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
// import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class DescribeTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): string {
// todo
    return "abap.statements.describe();";
  }

}