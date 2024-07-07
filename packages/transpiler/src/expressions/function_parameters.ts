import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

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

    const ret = [];
    for (const p of Object.keys(params)) {
      ret.push(p + ": " + params[p]);
    }

    return new Chunk(`{${ret.join(", ")}}`);
  }

}