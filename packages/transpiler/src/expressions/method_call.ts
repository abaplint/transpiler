import {Nodes, Expressions, Visibility, ScopeType, BuiltIn} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallParamTranspiler} from "./method_call_param";
import {Chunk} from "../chunk";

export class MethodCallTranspiler implements IExpressionTranspiler {
  private readonly unqualified: boolean;

  public constructor(unqualified = false) {
    this.unqualified = unqualified;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    let post = "";

    const nameToken = node.findDirectExpression(Expressions.MethodName)?.getFirstToken();
    if(nameToken === undefined) {
      throw new Error("MethodCallTranspiler, name not found");
    }

    const scope = traversal.findCurrentScopeByToken(nameToken);
    const m = traversal.findMethodReference(nameToken, scope);

    let name = nameToken.getStr().toLowerCase();
    // abaplint sometimes fails to register the builtin method reference, eg. when the call
    // follows a REDUCE expression, so fall back to matching against the known builtin methods.
    // Only do this for unqualified calls, ie. not calls on an object reference like "obj->count( )"
    const builtinFallback = this.unqualified && m === undefined
      ? BuiltIn.searchBuiltin(name) as abaplint.Types.MethodDefinition | undefined
      : undefined;
    const isBuiltin = traversal.isBuiltinMethod(nameToken) || builtinFallback !== undefined;
    if (isBuiltin) {
      // todo: this is not correct, the method name might be shadowed
      name = "abap.builtin." + name + "(";
      if (name === "abap.builtin.line_exists(" || name === "abap.builtin.line_index(") {
        name = `await ` + name;
        name += "async () => {";
        post = "}";
      }
    } else if (m?.name) {
      name = m.name.toLowerCase();
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
    } else {
      // todo: this should never happen?
      name = Traversal.escapeNamespace(name.replace("~", "$"))!;
      name = name + "(";
    }


    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const ret = new Chunk();
    ret.append(name, nameToken, traversal);
    ret.appendChunk(new MethodCallParamTranspiler(m?.def ?? builtinFallback).transpile(step, traversal));
    ret.appendString(post + ")");

    return ret;
  }

}