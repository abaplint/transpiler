import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";

export class ComponentCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
// todo, this is still not correct
    let ret = "";
    for(const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        const cond = traversal.traverse(c).getCode();
        ret += ret === "" ? cond : cond.replace(/^\(i\) => \{return /,"");
      } else if (c instanceof abaplint.Nodes.TokenNode) {
        switch (c.get().getStr()) {
          case "AND":
            ret = ret.replace(/;\}$/," && ");
            break;
          case "OR":
            ret = ret.replace(/;\}$/," || ");
            break;
          case "(":
            ret += "(";
            break;
          case ")":
            ret = ret.replace(/;\}$/,");}");
            break;
          default:
            // todo, runtime error
        }
      }
    }
    return new Chunk(ret);
  }
}