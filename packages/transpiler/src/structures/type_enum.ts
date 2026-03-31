import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TypeEnumTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, _traversal: Traversal): Chunk {
    const begin = node.findDirectStatement(abaplint.Statements.TypeEnumBegin);
    if (begin === undefined) {
      return new Chunk("");
    }

    const nsNames = begin.findAllExpressions(abaplint.Expressions.NamespaceSimpleName);
    if (nsNames.length < 2) {
      return new Chunk("");
    }

    const structureName = nsNames[1].concatTokens().toLowerCase();

    const values: string[] = [];
    for (const t of node.findDirectStatements(abaplint.Statements.Type)) {
      const name = t.findFirstExpression(abaplint.Expressions.NamespaceSimpleName);
      if (name) {
        values.push(name.concatTokens().toLowerCase());
      }
    }

    const fields = values.map(v =>
      `"${v}": new abap.types.String()`
    ).join(",\n");

    let ret = `let ${Traversal.prefixVariable(structureName)} = new abap.types.Structure({
${fields}});\n`;

    for (const v of values) {
      ret += `${Traversal.prefixVariable(structureName)}.get().${v}.set("${v.toUpperCase()}");\n`;
    }

    return new Chunk(ret);
  }

}
