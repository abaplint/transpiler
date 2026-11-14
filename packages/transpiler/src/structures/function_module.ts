import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {InlineDeclarations} from "../inline";

export class FunctionModuleTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();
    let name: string | undefined = "";
    let headers: string | undefined = undefined;
    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Statements.FunctionModule && c instanceof abaplint.Nodes.StatementNode) {
        name = c.findDirectExpression(abaplint.Expressions.Field)?.concatTokens().toLowerCase();
        if (name === undefined) {
          name = "FunctionModuleTranspilerNameNotFound";
        }
        chunk.append(`async function ${Traversal.escapeNamespace(name)}(INPUT) {\n`, c, traversal);
        const signature = this.findSignature(traversal, name, c);
        chunk.appendString(signature.code);
        if (signature.tables.length > 0) {
          // TABLES parameters have header lines, also when the caller's table has none, eg. called from a class:
          // the table gets one for the call
          headers = UniqueIdentifier.get();
          chunk.appendString(`const ${headers} = [${signature.tables.join(", ")}].map(t => t instanceof abap.types.FieldSymbol ? t.getPointer() : t)` +
            ".filter(t => t?.addHeaderLine?.() === true);\ntry {\n");
        }
        chunk.appendString(InlineDeclarations.buildDeclarations(node, traversal));
      } else if (c.get() instanceof abaplint.Statements.EndFunction) {
        if (headers !== undefined) {
          chunk.appendString(`} finally {\nfor (const t of ${headers}) { t.removeHeaderLine(); }\n}\n`);
        }
        chunk.append("}\n", c, traversal);
        chunk.appendString(`abap.FunctionModules['${name.toUpperCase()}'] = ${Traversal.escapeNamespace(name)};\n`);
      } else {
        chunk.appendChunk(traversal.traverse(c));
      }
    }
    UniqueIdentifier.resetIndexBackup();
    return chunk;
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
    const tables: string[] = [];
    for (const p of module.getParameters()) {
      ret += `// ${p.direction} ${p.name} ${p.type} ${p.optional}\n`;
      let direction: string = p.direction;
      if (direction === "importing") {
        direction = "exporting";
      } else if (direction === "exporting") {
        direction = "importing";
      }
      // note: all directions are optional
      const name = p.name.toLowerCase();
      // a parameter named like a JavaScript keyword, RETURN on every BAPI,
      // is declared the way the function body refers to it
      const variable = Traversal.prefixVariable(name);
      ret += `let ${variable} = INPUT.${direction}?.${name};\n`;
      if (direction === "tables") {
        tables.push(variable);
      }

      const type = scope?.findVariable(name)?.getType();
      if (type !== undefined && p.optional === true) {
        // todo, set DEFAULT value
        ret += `if (${variable} === undefined) {
  ${variable} = ${traversal.toType(type)};
}\n`;
      }

    }
    return {code: ret, tables};
  }

}
