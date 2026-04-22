import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {ComponentChainTranspiler} from "./component_chain";
import {UniqueIdentifier} from "../unique_identifier";

export class CorrespondingBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("CorrespondingBodyTranspiler, Expected TypeNameOrInfer");
    }
    const type = new TypeNameOrInfer().findType(typ, traversal);
    let target = TranspileTypes.toType(type);
    let source: Chunk | undefined;
    const isTableType = type instanceof BasicTypes.TableType;

    type mappingType = {
      componentName: Nodes.ExpressionNode | undefined,
      componentChain: Nodes.ExpressionNode | undefined,
    };
    const mapping: mappingType[] = [];

    for (const child of body.getChildren()) {
      const c = child.get();
      if (c instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        source = traversal.traverse(child);
      } else if (c instanceof Expressions.CorrespondingBodyBase && child instanceof Nodes.ExpressionNode) {
        source = traversal.traverse(child?.findDirectExpression(Expressions.Source));
        target = `abap.statements.moveCorresponding(${source!.getCode()}, ${target})`;
      } else if (c instanceof Expressions.CorrespondingBodyMapping && child instanceof Nodes.ExpressionNode) {
        let mappingRow: mappingType = {componentName: undefined, componentChain: undefined};
        for (const cc of child.getChildren()) {
          if (cc.get() instanceof Expressions.ComponentName) {
            mappingRow.componentName = cc as Nodes.ExpressionNode;
          } else if (cc.get() instanceof Expressions.ComponentChain) {
            mappingRow.componentChain = cc as Nodes.ExpressionNode;
            mapping.push(mappingRow);
            mappingRow = {componentName: undefined, componentChain: undefined};
          }
        }
      } else {
        throw new Error("CorrespondingBodyTranspiler, todo, " + c.constructor.name + ", " + body.concatTokens());
      }
    }

    const ret = new Chunk();
    const id = UniqueIdentifier.get();
    const sourceId = UniqueIdentifier.get();
    ret.appendString("(await (async () => {\n");
    ret.appendString(`const ${id} = ${target};\n`);
    ret.appendString(`const ${sourceId} = ${source!.getCode()};\n`);

    if (isTableType) {
      const rowTargetType = TranspileTypes.toType(type.getRowType());
      const sourceRowId = UniqueIdentifier.get();
      const targetRowId = UniqueIdentifier.get();
      ret.appendString(`for (const ${sourceRowId} of ${sourceId}.array()) {\n`);
      ret.appendString(`const ${targetRowId} = ${rowTargetType};\n`);
      ret.appendString(`abap.statements.moveCorresponding(${sourceRowId}, ${targetRowId});\n`);

      for (const map of mapping) {
        const componentName = map.componentName!.concatTokens().toLowerCase();
        const chain = new ComponentChainTranspiler().transpile(map.componentChain!, traversal).getCode();
        ret.appendString(`${targetRowId}.get().${componentName}.set(${sourceRowId}.get().${chain});\n`);
      }

      ret.appendString(`abap.statements.insertInternal({table: ${id}, data: ${targetRowId}});\n`);
      ret.appendString("}\n");
    } else {
      ret.appendString(`abap.statements.moveCorresponding(${sourceId}, ${id});\n`);

      for (const map of mapping) {
        const componentName = map.componentName!.concatTokens().toLowerCase();
        const chain = new ComponentChainTranspiler().transpile(map.componentChain!, traversal).getCode();
        ret.appendString(`${id}.get().${componentName}.set(${sourceId}.get().${chain});\n`);
      }
    }

    ret.appendString("return " + id + ";\n");
    ret.appendString("})())");

    return ret;
  }

}