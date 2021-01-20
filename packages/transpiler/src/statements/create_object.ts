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
//    const n = target + " = new abap.types.ABAPObject();\n";
    return target + ".set(await (new " + name + "()).constructor_(" + para + "));";
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

    const target = node.findDirectExpression(abaplint.Expressions.Target);
    const type = traversal.determineType(node, scope);
    if (type === undefined) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not found in scope`);
    } else if (!(type instanceof abaplint.BasicTypes.ObjectReferenceType)) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not a object reference`);
    }

    return type.getIdentifierName();
  }

}