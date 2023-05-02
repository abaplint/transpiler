import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler.js";
import {Traversal} from "../traversal.js";
import {DataTranspiler as DataStatementTranspiler} from "../statements/index.js";
import {Chunk} from "../chunk.js";

export class DataTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const begin = node.findDirectStatement(abaplint.Statements.DataBegin);
    if (begin === undefined) {
      return new Chunk("");
    }

    const topName = begin.findDirectExpression(abaplint.Expressions.DefinitionName)?.concatTokens().toLowerCase();
    const chunk = new DataStatementTranspiler().transpile(begin, traversal);

    for (const d of node.findDirectStatements(abaplint.Statements.Data)) {
      const subName = d.findFirstExpression(abaplint.Expressions.DefinitionName)?.concatTokens().toLowerCase();
      if (subName && topName) {
        chunk.appendString(DataStatementTranspiler.buildValue(d, topName + ".get()." + subName, traversal));
      }
    }

    chunk.appendString("\n");
    return chunk;
  }

}