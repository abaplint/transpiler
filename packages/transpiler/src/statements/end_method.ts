import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class EndMethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.getFirstToken();

    const scope = traversal.getSpaghetti().lookupPosition(token.getStart(), traversal.getFilename());
    if (scope === undefined) {
      throw new Error("EndMethodTranspiler, scope not found");
    }

    let returning: string = "";
    for (const v of scope.getData().vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        returning = "return " + v.name + ";\n";
      }
    }

    return returning + "}";
  }

}