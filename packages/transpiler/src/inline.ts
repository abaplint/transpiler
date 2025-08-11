import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";

export class InlineDeclarations {

  public static buildDeclarations(node: abaplint.Nodes.StructureNode, _traversal: Traversal): string {
    const expressions = node.findAllExpressionsRecursive(abaplint.Expressions.InlineData);
    const result = "";
    for (const expression of expressions) {
      console.dir(expression.concatTokens());

//      TranspileTypes.declare(t);
    }
    return result;
  }

}