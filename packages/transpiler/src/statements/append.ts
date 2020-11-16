import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../types";
import {UniqueIdentifier} from "../unique_identifier";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    const s = node.findDirectExpression(abaplint.Expressions.SimpleSource);
    if (s) {
      options.push("source: " + new SourceTranspiler().transpile(s, traversal));
    }

    if (node.concatTokens().toUpperCase().includes("INITIAL LINE")) {
      const token = node.findDirectExpression(abaplint.Expressions.Target)?.getFirstToken();
      if (token === undefined) {
        throw new Error("AppendTranspiler, token not found");
      }

      const scope = traversal.getSpaghetti().lookupPosition(token.getStart(), traversal.getFilename());
      if (scope === undefined) {
        throw new Error("AppendTranspiler, scope not found");
      }

      const found = scope.findVariable(token.getStr())?.getType();
      if (found === undefined) {
        throw new Error("AppendTranspiler, var not found, \"" + token.getStr() + "\"");
      } else if (!(found instanceof abaplint.BasicTypes.TableType)) {
        throw new Error("AppendTranspiler, not a table type");
      }

      const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.FieldSymbol));
      const unique = UniqueIdentifier.get();

      return "let " + unique + " = " + new TranspileTypes().toType(found.getRowType()) + "\n" +
        target + " = " + unique + ";\n" +
        token.getStr() + ".append(" + unique + ");";
    } else {
      options.push("target: " + traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)));
      return "abap.statements.append({" + options.join(", ") + "});";
    }

  }

}