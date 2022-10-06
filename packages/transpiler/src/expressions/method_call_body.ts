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

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.TokenNode) {
        // PARAMETER-TABLE
        continue;
      } else if (c.get() instanceof abaplint.Expressions.MethodCallParam) {
        ret.appendChunk(new MethodCallParamTranspiler(this.m).transpile(c, traversal));
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }

    return ret;
  }

}