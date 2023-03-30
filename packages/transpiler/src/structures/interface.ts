import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../transpile_types";
import {ConstantTranspiler, FieldChainTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class InterfaceTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    let ret = "";
    let name: string | undefined;
    const def = traversal.getInterfaceDefinition(node.getFirstToken());
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.Interface) {
        name = c.findDirectExpression(abaplint.Expressions.InterfaceName)?.getFirstToken().getStr().toLowerCase();
        name = Traversal.escapeNamespace(name);
        ret += `class ${name} {\n`;
        ret += `static INTERNAL_TYPE = 'INTF';\n`;
        ret += `static ATTRIBUTES = {${traversal.buildAttributes(def)}};\n`;
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndInterface) {
        ret += "}\n";
        ret += traversal.registerClassOrInterface(def);
      }
    }
    ret += this.buildConstants(node.findFirstExpression(abaplint.Expressions.InterfaceName), traversal);
    ret += this.buildTypes(def);

    return new Chunk(ret);
  }

  private buildTypes(idef: abaplint.IInterfaceDefinition | undefined): string {
    if (idef === undefined) {
      return "";
    }

    const prefix = Traversal.escapeNamespace(idef.getName().toLowerCase()) + ".";
    let ret = "";
    for (const ty of idef.getTypeDefinitions().getAll()) {
      ret += new TranspileTypes().declareStaticSkipVoid(prefix, ty.type);
    }
    return ret;
  }

  private buildConstants(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const vars = scope?.getData().vars;
    if (vars === undefined || Object.keys(vars).length === 0) {
      return "";
    }
    let ret = "\n";
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.Static) === false
          || identifier.getMeta().includes(abaplint.IdentifierMeta.ReadOnly) === false) {
        continue;
      }
      const interfaceName = Traversal.escapeNamespace(node.getFirstToken().getStr().toLowerCase());
      const name = interfaceName + "." + interfaceName + "$" + n.toLowerCase();
      ret += name + " = " + new TranspileTypes().toType(identifier.getType()) + ";\n";

      const constantStatement = traversal.findStatementInFile(identifier.getStart());
      const valExpression = constantStatement?.findFirstExpression(abaplint.Expressions.Value);
      if (valExpression?.getChildren()[1].get() instanceof abaplint.Expressions.SimpleFieldChain) {
        const s = new FieldChainTranspiler().transpile(
          valExpression.getChildren()[1] as abaplint.Nodes.ExpressionNode, traversal, false).getCode();
        const e = new ConstantTranspiler().escape(s);
        ret += name + ".set(" + e + ");\n";
        continue;
      }

      ret += traversal.setValues(identifier, name);
    }
    return ret;
  }

}