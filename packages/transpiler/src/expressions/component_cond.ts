import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import * as abaplint from "@abaplint/core";

export class ComponentCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
// todo, this is still not correct
    let ret = "";
    for(const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        const cond = traversal.traverse(c);
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
    return ret;
  }
}