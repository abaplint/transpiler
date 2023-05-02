import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SourceFieldTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const name = traversal.prefixAndName(node.getFirstToken(), traversal.getFilename()).replace("~", "$");
    ret.append(Traversal.escapeNamespace(name)!, node, traversal);

    return ret;
  }

}