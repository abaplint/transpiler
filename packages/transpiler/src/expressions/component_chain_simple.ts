import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
// todo
    return node.getFirstToken().getStr();
  }

}