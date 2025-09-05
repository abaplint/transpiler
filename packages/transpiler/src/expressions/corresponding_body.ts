import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";

export class CorrespondingBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("CorrespondingBodyTranspiler, Expected TypeNameOrInfer");
    }
    const type = new TypeNameOrInfer().findType(typ, traversal);
    let target = TranspileTypes.toType(type);
    let source: Chunk | undefined;

    type mappingType = {
      componentName: Nodes.ExpressionNode | undefined,
      componentChain: Nodes.ExpressionNode | undefined,
    };
    const mapping: mappingType[] = [];
    const mappingRow: mappingType = {componentName: undefined, componentChain: undefined};

    for (const child of body.getChildren()) {
      const c = child.get();
      if (c instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        source = traversal.traverse(child);
      } else if (c instanceof Expressions.CorrespondingBodyBase && child instanceof Nodes.ExpressionNode) {
        source = traversal.traverse(child?.findDirectExpression(Expressions.Source));
        target = `abap.statements.moveCorresponding(${source!.getCode()}, ${target})`;
      } else if (c instanceof Expressions.CorrespondingBodyMapping && child instanceof Nodes.ExpressionNode) {
        for (const cc of child.getChildren()) {
          if (cc.get() instanceof Expressions.ComponentName) {
            mappingRow.componentName = cc as Nodes.ExpressionNode;
          } else if (cc.get() instanceof Expressions.ComponentChain) {
            mappingRow.componentChain = cc as Nodes.ExpressionNode;
            mapping.push(mappingRow);
          }
        }
      } else {
        throw new Error("CorrespondingBodyTranspiler, todo, " + c.constructor.name + ", " + body.concatTokens());
      }
    }

    const ret = new Chunk();
    ret.appendString(`abap.statements.moveCorresponding(${source!.getCode()}, ${target})`);
    return ret;
  }

}