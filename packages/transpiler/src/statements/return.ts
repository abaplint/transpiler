import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ReturnTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let extra = "";
    const scope = traversal.findCurrentScope(node.getFirstToken());
    for (const v of scope?.getData().vars || []) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        extra = " " + v.name;
      }
    }

    return "return" + extra + ";";
  }

}