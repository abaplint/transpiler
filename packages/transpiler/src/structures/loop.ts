import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AtFirstTranspiler} from "./at_first";
import {AtLastTranspiler} from "./at_last";


export class LoopTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    let atFirst: Chunk | undefined = undefined;
    let atLast: Chunk | undefined = undefined;

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StructureNode && c.get() instanceof abaplint.Structures.Body) {
        for (const b of c.getChildren()) {
          for (const n of b.getChildren()) {
            if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtFirst) {
              atFirst = new AtFirstTranspiler().transpile(n, traversal);
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
    if (atFirst) {
      atted.appendChunk(atFirst);
    }

    atted.appendChunk(ret);

    if (atLast) {
      atted.appendChunk(atLast);
    }

    return atted;
  }

}