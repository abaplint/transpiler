import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SourceFieldTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const name = traversal.prefixAndName(node.getFirstToken(), traversal.getFilename()).replace("~", "$");
    ret.append(Traversal.escapeNamespace(name)!, node, traversal);

    return ret;
  }

}