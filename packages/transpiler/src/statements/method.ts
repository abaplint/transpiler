import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {UniqueIdentifier} from "../unique_identifier";

export class MethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const token = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    const name = token.getStr();

    const scope = spaghetti.lookupPosition(token.getStart(), filename);
    if (scope === undefined) {
      throw new Error("MethodTranspiler, scope not found");
    } else if (scope.getIdentifier().sname !== name) {
      throw new Error("MethodTranspiler, wrong scope found");
    }

    const parameterNames: string[] = [];
    for (const v of scope.getData().vars) {
      if (v.identifier.getMeta() === abaplint.IdentifierMeta.MethodImporting
          || v.identifier.getMeta() === abaplint.IdentifierMeta.MethodChanging
          || v.identifier.getMeta() === abaplint.IdentifierMeta.MethodExporting) {
        parameterNames.push(v.name);
      }
    }

    let unique = "";
    let after = "";
    if (parameterNames.length > 0) {
      unique = UniqueIdentifier.get();
      for (const p of parameterNames) {
        after = after + "\nlet " + p + " = " + unique + "." + p + ";";
      }
    }

    return name + "(" + unique + ") {" + after;
  }

}