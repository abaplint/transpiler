import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {UniqueIdentifier} from "../unique_identifier";

export class FunctionModuleTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    let r = "";
    let name: string | undefined = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Statements.FunctionModule && c instanceof abaplint.Nodes.StatementNode) {
        name = c.findDirectExpression(abaplint.Expressions.Field)?.concatTokens().toLowerCase();
        if (name === undefined) {
          name = "FunctionModuleTranspilerNameNotFound";
        }
        r += `async function ${Traversal.escapeNamespace(name)}(INPUT) {\n`;
        r += this.findSignature(traversal, name, c);
      } else if (c.get() instanceof abaplint.Statements.EndFunction) {
        r += "}\n";
        r += `abap.FunctionModules['${name.toUpperCase()}'] = ${Traversal.escapeNamespace(name)};\n`;
      } else {
        r += traversal.traverse(c).getCode();
      }
    }
    UniqueIdentifier.resetIndexBackup();
    return new Chunk(r);
  }

//////////////////////

  private findSignature(traversal: Traversal, name: string, node: abaplint.Nodes.StatementNode) {
    const group = traversal.getCurrentObject() as abaplint.Objects.FunctionGroup | undefined;
    if (group === undefined) {
      throw "FunctionModuleTranspilerGroupNotFound";
    }

    const module = group.getModule(name);
    if (module === undefined) {
      throw "FunctionModuleTranspilerModuleNotFound";
    }

    const scope = traversal.findCurrentScopeByToken(node.getLastToken());

    let ret = "";
    for (const p of module.getParameters()) {
      ret += `// ${p.direction} ${p.name} ${p.type} ${p.optional}\n`;
      let direction: string = p.direction;
      if (direction === "importing") {
        direction = "exporting";
      } else if (direction === "exporting") {
        direction = "importing";
      }
      // note: all directions are optional
      let name = p.name.toLowerCase();
      if (traversal.options?.keywords?.some(a => a === name)) {
        name += "_";
      }
      ret += `let ${name} = INPUT.${direction}?.${name};\n`;

      const type = scope?.findVariable(name)?.getType();
      if (type !== undefined && p.optional === true) {
        // todo, set DEFAULT value
        ret += `if (${name} === undefined) {
  ${name} = ${TranspileTypes.toType(type)};
}\n`;
      }

    }
    return ret;
  }

}