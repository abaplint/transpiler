import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CreateObjectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());
    if (scope === undefined) {
      throw new Error("CreateObjectTranspiler, unable to lookup position");
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const found = scope.findVariable(target);
    if (found === undefined) {
// todo, chained stuff?
      throw new Error("CreateObjectTranspiler, target variable not found in scope, \"" + target + "\"");
    }

// todo, handle constructor parameters

    const obj = found.getType() as abaplint.BasicTypes.ObjectReferenceType;
    return target + ".set(new " + obj.getName() + "());";
  }

}