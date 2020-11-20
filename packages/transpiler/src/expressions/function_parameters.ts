import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class FunctionParametersTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const params: {[index: string]: string} = {};

    const ex = node.findExpressionAfterToken("EXPORTING");
    if (ex) {
      params.exporting = traversal.traverse(ex);
    }
    const im = node.findExpressionAfterToken("IMPORTING");
    if (im) {
      params.importing = traversal.traverse(im);
    }
    const ta = node.findExpressionAfterToken("TABLES");
    if (ta) {
      params.tables = traversal.traverse(ta);
    }
    const ch = node.findExpressionAfterToken("CHANGING");
    if (ch) {
      params.changing = traversal.traverse(ch);
    }

    let ret = "";
    for (const p of Object.keys(params)) {
      if (ret === "") {
        ret += p + ": " + params[p];
      } else {
        ret += ", " + p + ": " + params[p];
      }
    }

    return `{${ret}}`;
  }

}