import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";


export class AtTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal, previous?: string, loopTarget?: string): Chunk {
    const ret = new Chunk();

    const atStatement = node.findDirectStatement(abaplint.Statements.At);
    const concat = atStatement?.concatTokens().toUpperCase();
    const name = atStatement
      ?.findDirectExpression(abaplint.Expressions.FieldSub)
      ?.concatTokens()
      ?.toLowerCase();

// todo: handle "table_line"

    if (concat?.startsWith("AT NEW ")) {
      // eslint-disable-next-line max-len
      ret.appendString(`if (${previous} === undefined || abap.compare.eq(${previous}.get().${name}, ${loopTarget}.get().${name}) === false) {\n`);
      const body = node.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }
      ret.appendString("}\n");
    } else if (concat?.startsWith("AT END OF ")) {
      // eslint-disable-next-line max-len
      ret.appendString(`if (${previous} === undefined || abap.compare.eq(${previous}.get().${name}, ${loopTarget}.get().${name}) === false) {\n`);
      const body = node.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }
      ret.appendString("}\n");
    }

    return ret;
  }

}