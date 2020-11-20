import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";

export class FunctionModuleTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let r = "";
    let name: string | undefined = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Statements.FunctionModule && c instanceof abaplint.Nodes.StatementNode) {
        name = c.findDirectExpression(abaplint.Expressions.Field)?.concatTokens().toLowerCase();
        if (name === undefined) {
          name = "FunctionModuleTranspilerNameNotFound";
        }
        r += `function ${name}(input) {\n`;
      } else if (c.get() instanceof abaplint.Statements.EndFunction) {
        r += "}\n";
        r += `abap.FunctionModules['${name.toUpperCase()}'] = ${name};\n`;
      } else {
        r += traversal.traverse(c);
      }
    }
    return r;
  }

}