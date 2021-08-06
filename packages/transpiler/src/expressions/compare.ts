import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
// todo, this is not correct

    const concat = node.concatTokens().toUpperCase();

    let pre = concat.startsWith("NOT ") ? "!" : "";

    const sources = node.findDirectExpressions(Expressions.Source);
    if (sources.length === 1) {
      const s0 = traversal.traverse(sources[0]);

      if ((concat.startsWith("NOT ") && concat.endsWith(" IS INITIAL"))
          || concat.endsWith("IS NOT INITIAL")) {
        return new Chunk().appendString("abap.compare.initial(").appendChunk(s0).appendString(") === false");
      } else if (concat.endsWith("IS INITIAL")) {
        return new Chunk().appendString("abap.compare.initial(").appendChunk(s0).appendString(")");
      }

      if ((concat.startsWith("NOT ") && concat.endsWith(" IS BOUND"))
          || concat.endsWith("IS NOT BOUND")) {
        return new Chunk().appendString("abap.compare.initial(").appendChunk(s0).appendString(")");
      } else if (concat.endsWith("IS BOUND")) {
        return new Chunk().appendString("abap.compare.initial(").appendChunk(s0).appendString(") === false");
      }

      if ((concat.startsWith("NOT ") && concat.endsWith(" IS ASSIGNED"))
          || concat.endsWith("IS NOT ASSIGNED")) {
        return new Chunk().appendString("abap.compare.assigned(").appendChunk(s0).appendString(") === false");
      } else if (concat.endsWith("IS ASSIGNED")) {
        return new Chunk().appendString("abap.compare.assigned(").appendChunk(s0).appendString(")");
      }

      if (concat.endsWith(" IS SUPPLIED")) {
        return new Chunk().appendString(pre + "INPUT && INPUT." + concat.replace(" IS SUPPLIED", "").toLowerCase());
      }
    } else if (sources.length === 2 && node.findDirectTokenByText("IN")) {
      if (concat.search(" NOT IN ") >= 0) {
        pre = pre === "!" ? "" : "!";
      }
      const s0 = traversal.traverse(sources[0]);
      const s1 = traversal.traverse(sources[1]);
      return new Chunk().appendString(pre + "abap.compare.in(").join([s0, s1]).appendString(")");
    } else if (sources.length === 2) {
      const operator = traversal.traverse(node.findFirstExpression(Expressions.CompareOperator));
      const s0 = traversal.traverse(sources[0]);
      const s1 = traversal.traverse(sources[1]);
      return new Chunk().appendString(pre + "abap.compare.").appendChunk(operator).appendString("(").join([s0, s1]).appendString(")");
    } else if (sources.length === 3 && node.findDirectTokenByText("BETWEEN")) {
      if (concat.search(" NOT BETWEEN ") >= 0) {
        pre = pre === "!" ? "" : "!";
      }
      const s0 = traversal.traverse(sources[0]);
      const s1 = traversal.traverse(sources[1]);
      const s2 = traversal.traverse(sources[2]);
      return new Chunk().appendString(pre + "abap.compare.between(").join([s0, s1, s2]).appendString(")");
    }

    return new Chunk("CompareTodo");
  }

}