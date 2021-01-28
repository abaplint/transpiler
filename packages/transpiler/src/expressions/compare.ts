import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class CompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
// todo, this is not correct

    const concat = node.concatTokens().toUpperCase();

    let pre = concat.startsWith("NOT") ? "!" : "";

    const sources = node.findDirectExpressions(Expressions.Source);
    if (sources.length === 1) {
      const s0 = traversal.traverse(sources[0]);

      if ((concat.startsWith("NOT") && concat.endsWith("IS INITIAL"))
          || concat.endsWith("IS NOT INITIAL")) {
        return "abap.compare.initial(" + s0 + ") === false";
      } else if (concat.endsWith("IS INITIAL")) {
        return "abap.compare.initial(" + s0 + ")";
      }

      if ((concat.startsWith("NOT") && concat.endsWith("IS BOUND"))
          || concat.endsWith("IS NOT BOUND")) {
        return "abap.compare.initial(" + s0 + ")";
      } else if (concat.endsWith("IS BOUND")) {
        return "abap.compare.initial(" + s0 + ") === false";
      }

      if ((concat.startsWith("NOT") && concat.endsWith("IS ASSIGNED"))
          || concat.endsWith("IS NOT ASSIGNED")) {
        return "abap.compare.assigned(" + s0 + ") === false";
      } else if (concat.endsWith("IS ASSIGNED")) {
        return "abap.compare.assigned(" + s0 + ")";
      }

      if (concat.endsWith(" IS SUPPLIED")) {
        return pre + "INPUT && INPUT." + concat.replace(" IS SUPPLIED", "").toLowerCase();
      }

    } else if (sources.length === 2) {
      const operator = traversal.traverse(node.findFirstExpression(Expressions.CompareOperator));
      const s0 = traversal.traverse(sources[0]);
      const s1 = traversal.traverse(sources[1]);
      return pre + "abap.compare." + operator + "(" + s0 + ", " + s1 + ")";
    } else if (sources.length === 3 && node.findDirectTokenByText("BETWEEN")) {
      if (concat.search("NOT BETWEEN") >= 0) {
        pre = pre === "!" ? "" : "!";
      }
      const s0 = traversal.traverse(sources[0]);
      const s1 = traversal.traverse(sources[1]);
      const s2 = traversal.traverse(sources[2]);
      return pre + "abap.compare.between(" + s0 + ", " + s1 + ", " + s2 + ")";
    }

    return "CompareTodo";
  }

}