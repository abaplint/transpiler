import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";

export class ComponentCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for(const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        let cond = traversal.traverse(c).getCode();
        cond = cond.replace(/^\(I\) => \{return /,"");
        cond = cond.replace(/;\}$/, "");
        ret += cond;
      } else if (c instanceof abaplint.Nodes.TokenNode) {
        switch (c.get().getStr().toUpperCase()) {
          case "AND":
            ret += " && ";
            break;
          case "OR":
            ret += " || ";
            break;
          default:
            throw new Error("ComponentCondTranspiler, unexpected");
        }
      }
    }
    ret = `(I) => {return ${ret};}`;
    return new Chunk(ret);
  }
}