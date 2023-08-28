import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ComponentChainSimpleTranspiler} from "./component_chain_simple";

export class ComponentCompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const concat = node.concatTokens().toUpperCase();
    const pre = concat.startsWith("NOT") ? "!" : "";
    const componentExpression = node.findDirectExpression(Expressions.ComponentChainSimple);
    if (componentExpression === undefined) {
      throw new Error("unexpected, ComponentCompareTranspiler");
    }
    const component = new ComponentChainSimpleTranspiler("I.").transpile(componentExpression, traversal).getCode();

    if (node.findDirectExpression(Expressions.CompareOperator)) {
      const compare = traversal.traverse(node.findDirectExpression(Expressions.CompareOperator)).getCode();
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return ${pre}abap.compare.${compare}(${component}, ${source});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS INITIAL"))
        || concat.endsWith("IS NOT INITIAL")) {
      return new Chunk(`(I) => {return abap.compare.initial(${component}) === false;}`);
    } else if (concat.endsWith("IS INITIAL")) {
      return new Chunk(`(I) => {return abap.compare.initial(${component});}`);
    }

    if (concat.startsWith(componentExpression?.concatTokens().toUpperCase() + " IN ")) {
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return ${pre}abap.compare.in(${component}, ${source});}`);
    } else if (concat.startsWith(componentExpression?.concatTokens().toUpperCase() + " NOT IN ")) {
      const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();
      return new Chunk(`(I) => {return !abap.compare.in(${component}, ${source});}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS BOUND"))
        || concat.endsWith("IS NOT BOUND")) {
      return new Chunk(`(I) => {return abap.compare.initial(${component});}`);
    } else if (concat.endsWith("IS BOUND")) {
      return new Chunk(`(I) => {return abap.compare.initial(${component}) === false;}`);
    }

    if ((concat.startsWith("NOT") && concat.endsWith("IS ASSIGNED"))
        || concat.endsWith("IS NOT ASSIGNED")) {
      return new Chunk(`(I) => {return abap.compare.assigned(${component}) === false;}`);
    } else if (concat.endsWith("IS ASSIGNED")) {
      return new Chunk(`(I) => {return abap.compare.assigned(${component});}`);
    }

    return new Chunk("ComponentCompareTodo");
  }

}