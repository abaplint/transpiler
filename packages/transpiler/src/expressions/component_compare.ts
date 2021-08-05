import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ComponentCompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const concat = node.concatTokens();
    const pre = concat.startsWith("NOT") ? "!" : "";
    const component = traversal.traverse(node.findDirectExpression(Expressions.ComponentChainSimple)).getCode();

    if (node.findDirectExpression(Expressions.CompareOperator)) {
      const compare = traversal.traverse(node.findDirectExpression(Expressions.CompareOperator)).getCode();
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(i) => {return ${pre}abap.compare.${compare}(i.${component}, ${source});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS INITIAL"))
        || concat.endsWith("IS NOT INITIAL")) {
      return new Chunk(`(i) => {return abap.compare.initial(i.${component}) === false;}`);
    } else if (concat.endsWith("IS INITIAL")) {
      return new Chunk(`(i) => {return abap.compare.initial(i.${component});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS BOUND"))
        || concat.endsWith("IS NOT BOUND")) {
      return new Chunk(`(i) => {return abap.compare.initial(i.${component});}`);
    } else if (concat.endsWith("IS BOUND")) {
      return new Chunk(`(i) => {return abap.compare.initial(i.${component}) === false;}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS ASSIGNED"))
        || concat.endsWith("IS NOT ASSIGNED")) {
      return new Chunk(`(i) => {return abap.compare.assigned(i.${component}) === false;}`);
    } else if (concat.endsWith("IS ASSIGNED")) {
      return new Chunk(`(i) => {return abap.compare.assigned(i.${component});}`);
    }

    return new Chunk("ComponentCompareTodo");
  }

}