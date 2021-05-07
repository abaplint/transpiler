import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../types";

export class InterfaceTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let ret = "";
    let name: string | undefined;
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.Interface) {
        name = c.findDirectExpression(abaplint.Expressions.InterfaceName)?.getFirstToken().getStr().toLowerCase();
        ret += `class ${name} {\n`;
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndInterface) {
        ret += "}\n";
        ret += `abap.Classes['${name?.toUpperCase()}'] = ${name};\n`;
      }
    }
    ret += this.buildConstants(node.findFirstExpression(abaplint.Expressions.InterfaceName), traversal);

    return ret;
  }

  private buildConstants(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const scope = traversal.findCurrentScope(node.getFirstToken());
    const vars = scope?.getData().vars;
    if (vars === undefined || Object.keys(vars).length === 0) {
      return "";
    }
    let ret = "";
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.Static) === false
          || identifier.getMeta().includes(abaplint.IdentifierMeta.ReadOnly) === false) {
        continue;
      }
      const interfaceName = node.getFirstToken().getStr().toLowerCase();
      const name = interfaceName + "." + interfaceName + "$" + n.toLowerCase();
      ret += name + " = " + new TranspileTypes().toType(identifier.getType()) + ";\n";
      const val = identifier.getValue();
      if (typeof val === "string") {
        ret += name + ".set(" + identifier.getValue() + ");\n";
      } else if (typeof val === "object") {
        const a: any = val;
        for (const v of Object.keys(val)) {
          const s = a[v];
          ret += name + ".get()." + v + ".set(" + s + ");\n";
        }
      }
    }
    return ret;
  }

}