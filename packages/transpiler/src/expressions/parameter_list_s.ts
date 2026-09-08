import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterListSTranspiler implements IExpressionTranspiler {
  private readonly extra: string;

  /** @param extra appended as an additional field of the emitted object, eg. "result: 1" */
  public constructor(extra = "") {
    this.extra = extra;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const parameters: Chunk[] = [];

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        parameters.push(traversal.traverse(c));
      }
    }

    const post = this.extra === "" ? "}" : ", " + this.extra + "}";
    return new Chunk().appendString("{").join(parameters).appendString(post);
  }

}