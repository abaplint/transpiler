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

    let name = "";
    const dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.ConstantString);
    if (dynamic) {
      name = dynamic.getFirstToken().getStr();
      name = name.substring(1, name.length - 1);
    } else {
      name = this.findClassName(node, traversal);
    }

    let ret = "";
    const clas = traversal.lookupClassOrInterface(name, node.getFirstToken());
    if (dynamic) {
      ret += "if (" + clas + " === undefined) { throw 'cx_sy_create_object_error'; }\n";
    }
    ret += target + ".set(await (new " + clas + "()).constructor_(" + para + "));";

    return ret;
  }

  private findClassName(node: abaplint.Nodes.StatementNode, traversal: Traversal) {
    const c = node.findDirectExpression(abaplint.Expressions.ClassName);
    if (c) {
      return c.concatTokens();
    }

    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (scope === undefined) {
      throw new Error("CreateObjectTranspiler, unable to lookup position");
    }

    const target = node.findDirectExpression(abaplint.Expressions.Target);
    const type = traversal.determineType(node, scope);
    if (type === undefined) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not found in scope`);
    } else if (type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      return "object";
    } else if (!(type instanceof abaplint.BasicTypes.ObjectReferenceType)) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not a object reference`);
    }

    return type.getIdentifierName();
  }

}