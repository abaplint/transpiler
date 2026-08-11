import {Nodes, Expressions, Visibility, ScopeType, Types} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallParamTranspiler} from "./method_call_param";
import {Chunk} from "../chunk";

export class MethodCallTranspiler implements IExpressionTranspiler {
  private readonly postName: string;
  private readonly method: {def: Types.MethodDefinition, name: string} | undefined;

  /** @param postName inserted between the method name and the parameters, eg. ".bind(this)"
   *  @param method   the already resolved method reference, saves looking it up again */
  public constructor(postName = "", method?: {def: Types.MethodDefinition, name: string}) {
    this.postName = postName;
    this.method = method;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    let post = "";

    const nameToken = node.findDirectExpression(Expressions.MethodName)?.getFirstToken();
    if(nameToken === undefined) {
      throw new Error("MethodCallTranspiler, name not found");
    }

    const scope = traversal.findCurrentScopeByToken(nameToken);
    const m = this.method ?? traversal.findMethodReference(nameToken, scope);

    let name = nameToken.getStr().toLowerCase();
    if (traversal.isBuiltinMethod(nameToken)) {
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
      name = name + this.postName + "(";
    } else {
      // todo: this should never happen?
      name = Traversal.escapeNamespace(name.replace("~", "$"))!;
      name = name + this.postName + "(";
    }


    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const ret = new Chunk();
    ret.append(name, nameToken, traversal);
    ret.appendChunk(new MethodCallParamTranspiler(m?.def).transpile(step, traversal));
    ret.appendString(post + ")");

    return ret;
  }

}
