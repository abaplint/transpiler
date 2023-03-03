import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AtFirstTranspiler} from "./at_first";
import {AtLastTranspiler} from "./at_last";
import {UniqueIdentifier} from "../unique_identifier";


export class LoopTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    let pre = "";
    let atFirst: Chunk | undefined = undefined;
    let atLast: Chunk | undefined = undefined;

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StructureNode && c.get() instanceof abaplint.Structures.Body) {
        for (const b of c.getChildren()) {
          for (const n of b.getChildren()) {
            if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtFirst) {
              atFirst = new AtFirstTranspiler().transpile(n, traversal);
              const u = UniqueIdentifier.get();
              pre = "let " + u + " = false;\n";
              ret.appendString("if (" + u + " === false) {\n");
              ret.appendChunk(atFirst);
              ret.appendString(u + " = true;\n");
              ret.appendString("}\n");
            } else if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtLast) {
              atLast = new AtLastTranspiler().transpile(n, traversal);
            } else {
              ret.appendChunk(traversal.traverse(n));
            }
          }
        }
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }

    const atted = new Chunk();
    if (pre) {
      atted.appendString(pre);
    }

    atted.appendChunk(ret);

    if (atLast) {
      atted.appendString("if (abap.builtin.sy.get().subrc.get() === 0) {\n");
      atted.appendChunk(atLast);
      atted.appendString("}\n");
    }

    return atted;
  }

}