import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ComponentCompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const concat = node.concatTokens().toUpperCase();
    const pre = concat.startsWith("NOT") ? "!" : "";
    const component = traversal.traverse(node.findDirectExpression(Expressions.ComponentChainSimple)).getCode();

    if (node.findDirectExpression(Expressions.CompareOperator)) {
      const compare = traversal.traverse(node.findDirectExpression(Expressions.CompareOperator)).getCode();
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return ${pre}abap.compare.${compare}(I.${component}, ${source});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS INITIAL"))
        || concat.endsWith("IS NOT INITIAL")) {
      return new Chunk(`(I) => {return abap.compare.initial(I.${component}) === false;}`);
    } else if (concat.endsWith("IS INITIAL")) {
      return new Chunk(`(I) => {return abap.compare.initial(I.${component});}`);
    }

    if (concat.startsWith(component.toUpperCase() + " IN ")) {
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return ${pre}abap.compare.in(I.${component}, ${source});}`);
    } else if (concat.startsWith(component.toUpperCase() + " NOT IN ")) {
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return !abap.compare.in(I.${component}, ${source});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS BOUND"))
        || concat.endsWith("IS NOT BOUND")) {
      return new Chunk(`(I) => {return abap.compare.initial(I.${component});}`);
    } else if (concat.endsWith("IS BOUND")) {
      return new Chunk(`(I) => {return abap.compare.initial(I.${component}) === false;}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS ASSIGNED"))
        || concat.endsWith("IS NOT ASSIGNED")) {
      return new Chunk(`(I) => {return abap.compare.assigned(I.${component}) === false;}`);
    } else if (concat.endsWith("IS ASSIGNED")) {
      return new Chunk(`(I) => {return abap.compare.assigned(I.${component});}`);
    }

    return new Chunk("ComponentCompareTodo");
  }

}