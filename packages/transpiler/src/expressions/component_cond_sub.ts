import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";

export class ComponentCondSubTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for(const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        let cond = traversal.traverse(c).getCode();
        cond = cond.replace(/^\(i\) => \{return /,"");
        cond = cond.replace(/;\}$/, "");
        ret += cond;
      } else if (c instanceof abaplint.Nodes.TokenNode) {
        switch (c.get().getStr().toUpperCase()) {
          case "(":
            ret += "(";
            break;
          case ")":
            ret += ")";
            break;
          case "NOT":
            ret += "!";
            break;
          default:
            throw new Error("ComponentCondSubTranspiler, unexpected: " + node.concatTokens());
        }
      }
    }
    ret = `(i) => {return ${ret};}`;
    return new Chunk(ret);
  }
}