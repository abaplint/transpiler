import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {LoopTranspiler} from "../statements";
import {UniqueIdentifier} from "../unique_identifier";


export class AtTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode,
                   traversal: Traversal,
                   previous?: string,
                   loopTarget?: string,
                   tabix?: string,
                   loopStatement?: abaplint.Nodes.StatementNode): Chunk {

    const ret = new Chunk();

    const atStatement = node.findDirectStatement(abaplint.Statements.At);
    const concat = atStatement?.concatTokens().toUpperCase();
    const name = atStatement
      ?.findDirectExpression(abaplint.Expressions.FieldSub)
      ?.concatTokens()
      ?.toLowerCase();

    let suffix = `.get().${name}`;
    if (name === "table_line") {
      suffix = "";
    }

    if (concat?.startsWith("AT NEW ")) {
      ret.appendString(`if (${previous} === undefined || abap.compare.eq(${previous}${suffix}, ${loopTarget}${suffix}) === false) {\n`);
      const body = node.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }
      ret.appendString("}\n");
    } else if (concat?.startsWith("AT END OF ")) {
      const next = UniqueIdentifier.get();
      ret.appendString(`let ${next} = undefined;\n`);
      const loop = new LoopTranspiler({injectFrom: tabix, skipInto: true});
      ret.appendChunk(loop.transpile(loopStatement!, traversal));
      ret.appendString(`${next} = ${loop.getTarget()};\n`);
      ret.appendString(`break;\n`);
      ret.appendString(`}\n`);
      ret.appendString(`if (${next} === undefined || abap.compare.eq(${next}${suffix}, ${loopTarget}${suffix}) === false) {\n`);
      const body = node.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }
      ret.appendString("}\n");
    }

    return ret;
  }

}