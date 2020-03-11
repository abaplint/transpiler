import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class EndMethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const token = node.getFirstToken();

    const scope = spaghetti.lookupPosition(token.getStart(), filename);
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