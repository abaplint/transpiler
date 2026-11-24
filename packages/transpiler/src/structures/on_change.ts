import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

/** ON CHANGE OF f [OR g ...] (obsolete): the block runs when a field differs from what this statement saw the last
 * time it ran, the first time when a field is not initial; the runtime keeps the last values per statement */
export class OnChangeTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const on = node.findDirectStatement(abaplint.Statements.OnChange);
    if (on === undefined) {
      throw new Error("OnChangeTranspiler, statement not found");
    }
    const fields = on.findDirectExpressions(abaplint.Expressions.Target).map(t => traversal.traverse(t).getCode());
    const start = on.getFirstToken().getStart();
    const key = JSON.stringify(traversal.getFilename() + ":" + start.getRow() + ":" + start.getCol());
    ret.append(`if (abap.statements.onChange(${key}, [${fields.join(", ")}])) {\n`, on, traversal);
    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }
    const otherwise = node.findDirectStructure(abaplint.Structures.Else);
    if (otherwise) {
      ret.appendString("} else {\n");
      const elseBody = otherwise.findDirectStructure(abaplint.Structures.Body);
      if (elseBody) {
        ret.appendChunk(traversal.traverse(elseBody));
      }
    }
    ret.appendString("}\n");
    return ret;
  }

}
