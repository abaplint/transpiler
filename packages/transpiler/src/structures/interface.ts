import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../types";

export class InterfaceTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.Interface) {
        const name = c.findDirectExpression(abaplint.Expressions.InterfaceName)?.getFirstToken().getStr().toLowerCase();
        ret += `class ${name} {\n`;
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndInterface) {
        ret += "}\n";
      }
    }
    ret += this.buildConstants(node.findFirstExpression(abaplint.Expressions.InterfaceName), traversal);

    return ret;
  }

  private buildConstants(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());
    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "";
    for (const v of vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.Static) === false
          || v.identifier.getMeta().includes(abaplint.IdentifierMeta.ReadOnly) === false) {
        continue;
      }
      const name = node.getFirstToken().getStr().toLowerCase() + "." + v.name.toLocaleLowerCase().replace("~", "$");
      ret += name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
      const val = v.identifier.getValue();
      if (typeof val === "string") {
        ret += name + ".set(" + v.identifier.getValue() + ");\n";
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