import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class FunctionParametersTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const params: {[index: string]: string} = {};

    const ex = node.findExpressionAfterToken("EXPORTING");
    if (ex) {
      params.exporting = traversal.traverse(ex).getCode();
    }
    const im = node.findExpressionAfterToken("IMPORTING");
    if (im) {
      params.importing = traversal.traverse(im).getCode();
    }
    const ta = node.findExpressionAfterToken("TABLES");
    if (ta) {
      params.tables = traversal.traverse(ta).getCode();
    }
    const ch = node.findExpressionAfterToken("CHANGING");
    if (ch) {
      params.changing = traversal.traverse(ch).getCode();
    }

    let ret = "";
    for (const p of Object.keys(params)) {
      if (ret === "") {
        ret += p + ": " + params[p];
      } else {
        ret += ", " + p + ": " + params[p];
      }
    }

    return new Chunk(`{${ret}}`);
  }

}