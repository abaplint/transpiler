import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class LetTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(node.get() instanceof Expressions.Let)) {
      throw new Error("LetTranspiler, Expected Let");
    }

    const ret = new Chunk();

    for (const def of node.findAllExpressions(Expressions.InlineFieldDefinition)) {
      const nameToken = def.findDirectExpression(Expressions.Field);
      if (nameToken === undefined) {
        throw new Error("LetTranspiler, Expected Field");
      }
      const name = nameToken?.concatTokens();
      const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
      const variable = scope?.findVariable(name);
      if (variable === undefined) {
        throw new Error("LetTranspiler, Expected Variable");
      }
      ret.appendString(TranspileTypes.declare(variable));

      const source = def.findDirectExpression(Expressions.Source);
      if (source) {
        ret.appendString(name + `.set(${traversal.traverse(source).getCode()});`);
      }
    }

    return ret;
  }

}