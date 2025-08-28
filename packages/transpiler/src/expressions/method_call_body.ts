import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodCallParamTranspiler} from "./method_call_param";

export class MethodCallBodyTranspiler implements IExpressionTranspiler {

  private readonly m: abaplint.Types.MethodDefinition | undefined;

  public constructor(m?: abaplint.Types.MethodDefinition) {
    this.m = m;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    // let pt = false;

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.TokenNode) {
        /*
        if (c.concatTokens().toUpperCase() === "PARAMETER") {
          // PARAMETER-TABLE
          pt = true;
        } else if(c.concatTokens().toUpperCase() === "EXCEPTION") {
          // todo: handle EXCEPTION-TABLE, for now just produce valid javascript
          return ret;
        }
          */
      } else if (c.get() instanceof abaplint.Expressions.MethodCallParam) {
        ret.appendChunk(new MethodCallParamTranspiler(this.m).transpile(c, traversal));
      } else {
        ret.appendChunk(traversal.traverse(c));
        /*
        if (pt === true) {
          ret.appendString(".array().reduce((a, v) => ({ ...a, [v.get().name.get(
          ).toLowerCase().trimEnd()]: v.get().value.dereference()}), {})");
        }
        */
      }
    }

    return ret;
  }

}