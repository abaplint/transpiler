import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {ComponentChainTranspiler} from "./component_chain";
import {UniqueIdentifier} from "../unique_identifier";

type mappingType = {
  componentName: Nodes.ExpressionNode | undefined,
  componentChain: Nodes.ExpressionNode | undefined,
};

export class CorrespondingBodyTranspiler {

  /** genericTarget: code of the target the result is assigned to, used when the inferred type is generic,
   *  the result then gets the runtime type of the target, eg. "ct = CORRESPONDING #( ... )" with
   *  "CHANGING ct TYPE STANDARD TABLE" */
  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal, genericTarget?: string): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("CorrespondingBodyTranspiler, Expected TypeNameOrInfer");
    }
    const type = new TypeNameOrInfer().findType(typ, traversal);
    if (genericTarget !== undefined && type.isGeneric() === true) {
      return this.transpileGeneric(body, traversal, genericTarget);
    }
    const {source, base, mapping} = this.parseBody(body, traversal);
    let target = traversal.toType(type);
    if (base !== undefined) {
      target = `abap.statements.moveCorresponding(${base.getCode()}, ${target})`;
    }
    const isTableType = type instanceof BasicTypes.TableType;

    const ret = new Chunk();
    const id = UniqueIdentifier.get();
    const sourceId = UniqueIdentifier.get();
    ret.appendString("(await (async () => {\n");
    ret.appendString(`const ${id} = ${target};\n`);
    ret.appendString(`const ${sourceId} = ${source.getCode()};\n`);

    if (isTableType) {
      const rowTargetType = traversal.toType(type.getRowType());
      const sourceRowId = UniqueIdentifier.get();
      const targetRowId = UniqueIdentifier.get();
      ret.appendString(`for (const ${sourceRowId} of ${sourceId}.array()) {\n`);
      ret.appendString(`const ${targetRowId} = ${rowTargetType};\n`);
      ret.appendString(`abap.statements.moveCorresponding(${sourceRowId}, ${targetRowId});\n`);
      ret.appendString(this.mappingCode(mapping, targetRowId, sourceRowId, traversal));
      ret.appendString(`abap.statements.insertInternal({table: ${id}, data: ${targetRowId}});\n`);
      ret.appendString("}\n");
    } else {
      ret.appendString(`abap.statements.moveCorresponding(${sourceId}, ${id});\n`);
      ret.appendString(this.mappingCode(mapping, id, sourceId, traversal));
    }

    ret.appendString("return " + id + ";\n");
    ret.appendString("})())");

    return ret;
  }

  /** the type is only known at runtime, it is taken from the target, a table or a structure */
  private transpileGeneric(body: Nodes.ExpressionNode, traversal: Traversal, genericTarget: string): Chunk {
    const {source, base, mapping} = this.parseBody(body, traversal);

    const ret = new Chunk();
    const id = UniqueIdentifier.get();
    const sourceId = UniqueIdentifier.get();
    const sourceRowId = UniqueIdentifier.get();
    const targetRowId = UniqueIdentifier.get();
    ret.appendString("(await (async () => {\n");
    const pointer = `(${genericTarget} instanceof abap.types.FieldSymbol ? ${genericTarget}.getPointer() : ${genericTarget})`;
    ret.appendString(`const ${id} = ${pointer}.clone();\n`);
    ret.appendString(`${id}.clear();\n`);
    if (base !== undefined) {
      ret.appendString(`${id}.set(${base.getCode()});\n`);
    }
    ret.appendString(`const ${sourceId} = ${source.getCode()};\n`);
    ret.appendString(`if (${id} instanceof abap.types.Table || ${id} instanceof abap.types.HashedTable) {\n`);
    ret.appendString(`for (const ${sourceRowId} of ${sourceId}.array()) {\n`);
    ret.appendString(`const ${targetRowId} = ${id}.getRowType().clone();\n`);
    ret.appendString(`${targetRowId}.clear();\n`);
    ret.appendString(`abap.statements.moveCorresponding(${sourceRowId}, ${targetRowId});\n`);
    ret.appendString(this.mappingCode(mapping, targetRowId, sourceRowId, traversal));
    ret.appendString(`abap.statements.insertInternal({table: ${id}, data: ${targetRowId}});\n`);
    ret.appendString("}\n");
    ret.appendString("} else {\n");
    ret.appendString(`abap.statements.moveCorresponding(${sourceId}, ${id});\n`);
    ret.appendString(this.mappingCode(mapping, id, sourceId, traversal));
    ret.appendString("}\n");
    ret.appendString("return " + id + ";\n");
    ret.appendString("})())");

    return ret;
  }

  private parseBody(body: Nodes.ExpressionNode, traversal: Traversal) {
    let source: Chunk | undefined;
    let base: Chunk | undefined;
    const mapping: mappingType[] = [];

    for (const child of body.getChildren()) {
      const c = child.get();
      if (c instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        source = traversal.traverse(child);
      } else if (c instanceof Expressions.CorrespondingBodyBase && child instanceof Nodes.ExpressionNode) {
        base = traversal.traverse(child.findDirectExpression(Expressions.Source));
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

    return {source: source!, base, mapping};
  }

  private mappingCode(mapping: mappingType[], targetId: string, sourceId: string, traversal: Traversal): string {
    let code = "";
    for (const map of mapping) {
      const componentName = map.componentName!.concatTokens().toLowerCase();
      const chain = new ComponentChainTranspiler().transpile(map.componentChain!, traversal).getCode();
      code += `${targetId}.get().${componentName}.set(${sourceId}.get().${chain});\n`;
    }
    return code;
  }

}