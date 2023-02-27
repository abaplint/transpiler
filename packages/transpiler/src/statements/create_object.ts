import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "../expressions";

export class CreateObjectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    let para = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      para = traversal.traverse(parameters).getCode();
    }

    let name = "";
    let directGlobal = false;
    let dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.ConstantString);
    if (dynamic) {
      name = dynamic.getFirstToken().getStr();
      name = name.substring(1, name.length - 1);
    } else {
      dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.FieldChain);
      if (dynamic) {
        name = new FieldChainTranspiler(true).transpile(dynamic, traversal).getCode();
        directGlobal = true;
      }
    }

    if (name === "") {
      name = this.findClassName(node, traversal);
    }

    let ret = "";
    const clas = traversal.lookupClassOrInterface(name, node.getFirstToken(), directGlobal);
    const cx = traversal.lookupClassOrInterface("CX_SY_CREATE_OBJECT_ERROR", node.getFirstToken());
    if (dynamic) {
      ret += `if (${clas} === undefined) { throw new ${cx}; }\n`;
    }
    ret += target + ".set(await (new " + clas + "()).constructor_(" + para + "));";

    return new Chunk(ret);
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
    if (target === undefined) {
      throw new Error(`CreateObjectTranspiler, target not found`);
    }

    const type = traversal.determineType(node, scope);
    if (type === undefined) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not found in scope`);
    } else if (type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      return "object";
    } else if (!(type instanceof abaplint.BasicTypes.ObjectReferenceType)) {
      if (traversal.options?.unknownTypes !== "runtimeError") {
        throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not a object reference`);
      } else {
        return "RUNTIME_ERROR";
      }
    }

    return type.getIdentifierName();
  }

}