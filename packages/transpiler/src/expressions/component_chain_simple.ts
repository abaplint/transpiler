import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    for (const c of node.getChildren()) {
      const type = c.get();
      if (type instanceof Expressions.ComponentName) {
        let field = c.getFirstToken().getStr().toLowerCase();
        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        if (interfaceName && field.startsWith(interfaceName) === false) {
          field = interfaceName + "$" + field;
        }
        field = Traversal.escapeNamespace(field)!.replace("~", "$");
        ret.append(field, c, traversal);
      } else if (type instanceof Expressions.ArrowOrDash) {
        ret.append(".get().", c, traversal);
      }
    }
    return ret;
  }

}