import {Nodes, Expressions, Visibility, ScopeType} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallParamTranspiler} from "./method_call_param";
import {Chunk} from "../chunk";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const nameToken = node.findDirectExpression(Expressions.MethodName)?.getFirstToken();
    if(nameToken === undefined) {
      throw new Error("MethodCallTranspiler, name not found");
    }

    let name = nameToken.getStr().toLowerCase();
    if (traversal.isBuiltinMethod(nameToken)) {
      name = "abap.builtin." + name;
    }

    const scope = traversal.findCurrentScopeByToken(nameToken);
    // it might be aliased?
    const m = traversal.findMethodReference(nameToken, scope);
    if (m?.name && traversal.isBuiltinMethod(nameToken) === false) {
      name = m.name.toLowerCase();
    }

    name = Traversal.escapeNamespace(name.replace("~", "$"))!;

    if (m?.def.getVisibility() === Visibility.Private
        && m.def.isStatic() === false) {
      const id = scope?.getParent()?.getParent()?.getIdentifier();
      if (id?.stype === ScopeType.ClassImplementation
          && m.def.getClassName().toUpperCase() === id.sname.toUpperCase()) {
        name = "#" + name;
      } else {
        name = `FRIENDS_ACCESS_INSTANCE["${name}"]`;
      }
    }

    name = name + "(";

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const ret = new Chunk();
    ret.append(name, nameToken, traversal);
    ret.appendChunk(new MethodCallParamTranspiler(m?.def).transpile(step, traversal));
    ret.appendString(")");

    return ret;
  }

}