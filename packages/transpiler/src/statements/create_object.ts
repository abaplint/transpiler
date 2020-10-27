import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CreateObjectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {


    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    let para = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      para = traversal.traverse(parameters);
    }

    const name = this.findClassName(node, traversal);
    return target + ".set(new " + name + "(" + para + "));";
  }

  private findClassName(node: abaplint.Nodes.StatementNode, traversal: Traversal) {
    const c = node.findDirectExpression(abaplint.Expressions.ClassName);
    if (c) {
      return c.concatTokens();
    }

    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());
    if (scope === undefined) {
      throw new Error("CreateObjectTranspiler, unable to lookup position");
    }

    const type = traversal.determineType(node, scope);
    if (type === undefined) {
// todo, chained stuff?
      throw new Error("CreateObjectTranspiler, target variable not found in scope");
    }

    const obj = type as abaplint.BasicTypes.ObjectReferenceType;

    return obj.getIdentifierName();
  }

}