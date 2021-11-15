import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {UniqueIdentifier} from "../unique_identifier";
import {WhenTranspiler} from "../statements";
import {Chunk} from "../chunk";

export class CaseTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    // does not use switch(), as it break;'s EXITs
    const s = node.findDirectStatement(abaplint.Statements.Case)?.findDirectExpression(abaplint.Expressions.Source);
    if (s === undefined) {
      throw new Error("CASE, no Source found");
    }

    let first = true;
    const u = UniqueIdentifier.get();
    const ret = new Chunk();

    // this determines and copies the value
    ret.append("let " + u + " = ", node, traversal);
    ret.appendChunk(traversal.traverse(s));
    ret.append(";\n", node, traversal);

    for (const w of node.findDirectStructures(abaplint.Structures.When)) {
      for (const c of w.getChildren()) {
        if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.When) {
          if (first === true) {
            first = false;
            ret.appendString("if (");
          } else {
            ret.appendString("} else if (");
          }
          ret.appendChunk(new WhenTranspiler(u).transpile(c, traversal));
          ret.appendString(") {\n");
        } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.WhenOthers) {
          if (first === true) {
            continue;
          }
          ret.appendString("} else {\n");
        } else {
          ret.appendChunk(traversal.traverse(c)); // Normal
        }
      }
    }

    if (first === false) {
      ret.appendString("}\n");
    }

    return ret;
  }

}