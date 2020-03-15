import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {UniqueIdentifier} from "../unique_identifier";
import {TranspileTypes} from "../types";
import {Traversal} from "../traversal";

export class MethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    const name = token.getStr();

    const scope = traversal.getSpaghetti().lookupPosition(token.getStart(), traversal.getFilename());
    if (scope === undefined) {
      throw new Error("MethodTranspiler, scope not found");
    } else if (scope.getIdentifier().sname !== name) {
      throw new Error("MethodTranspiler, wrong scope found");
    }

    let after = "";
    let unique = "";
    for (const v of scope.getData().vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting)
          || v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodChanging)
          || v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodExporting)) {
        if (unique === "") {
          unique = UniqueIdentifier.get();
        }
        after = after + "\n" + new TranspileTypes().declare(v.identifier);
        after = after + "\nif (" + unique + " && " + unique + "." + v.name + ") {" + v.name + ".set(" + unique + "." + v.name + ");}";
      } else if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        after = after + "\n" + new TranspileTypes().declare(v.identifier);
      }
    }

    return name + "(" + unique + ") {" + after;
  }

}