import {Nodes, Expressions, Visibility} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallParamTranspiler} from "./method_call_param";
import {Chunk} from "../chunk";
import {FEATURE_FLAGS} from "../feature_flags";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const nameToken = node.findDirectExpression(Expressions.MethodName)?.getFirstToken();
    if(nameToken === undefined) {
      throw new Error("MethodCallTranspiler, name not found");
    }

    let name = nameToken.getStr().toLowerCase();
    if (traversal.isBuiltinMethod(nameToken)) {
      name = "abap.builtin." + name + "(";
    } else {
      name = name + "(";
    }

    const m = traversal.findMethodReference(nameToken, traversal.findCurrentScopeByToken(nameToken));
    if (m?.name && traversal.isBuiltinMethod(nameToken) === false) {
      name = m.name.toLowerCase() + "(";
    }
    if (FEATURE_FLAGS.private === true
        && m?.def.getVisibility() === Visibility.Private) {
      name = "#" + name;
    }

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const ret = new Chunk();
    ret.append(Traversal.escapeNamespace(name.replace("~", "$"))!, nameToken, traversal);
    ret.appendChunk(new MethodCallParamTranspiler(m?.def).transpile(step, traversal));
    ret.appendString(")");

    return ret;
  }

}