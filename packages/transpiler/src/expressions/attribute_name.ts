import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AttributeNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let concat = node.concatTokens().toLowerCase();

    // todo: this needs to also check the class name is correct
    const ref = traversal.findReadOrWriteReference(node.getFirstToken());
    if (ref instanceof abaplint.Types.ClassAttribute
        && ref.getVisibility() === abaplint.Visibility.Private) {
      concat = "#" + concat;
    }

    return new Chunk().append(concat, node, traversal);
  }

}