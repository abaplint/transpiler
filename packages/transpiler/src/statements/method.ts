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

    if (name.toUpperCase() === "CONSTRUCTOR") {
      after = traversal.buildConstructorContents(scope.getParent());
    }

    let unique = "";
    for (const v of scope.getData().vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting)
          || v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodChanging)
          || v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodExporting)) {
        if (unique === "") {
          unique = UniqueIdentifier.get();
        }
        after = after + new TranspileTypes().declare(v.identifier) + "\n";
        after = after + "if (" + unique + " && " + unique + "." + v.name + ") {" + v.name + ".set(" + unique + "." + v.name + ");}\n";
      } else if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        after = after + new TranspileTypes().declare(v.identifier) + "\n";
      }
    }

    if (after.length > 0) { // argh
      after = "\n" + after;
      after = after.substring(0, after.length - 1);
    }

    return name + "(" + unique + ") {" + after;
  }

}