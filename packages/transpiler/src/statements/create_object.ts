import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TargetTranspiler} from "../expressions";

export class CreateObjectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const scope = spaghetti.lookupPosition(node.getFirstToken().getStart(), filename);
    if (scope === undefined) {
      throw new Error("CreateObjectTranspiler, unable to lookup position");
    }

    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);

    const found = scope.findVariable(target);
    if (found === undefined) {
// todo, chained stuff?
      throw new Error("CreateObjectTranspiler, target variable not found in scope");
    }

// todo, handle constructor parameters

    const obj = found.getType() as abaplint.BasicTypes.ObjectReferenceType;
    return target + ".set(new " + obj.getName() + "());";
  }

}