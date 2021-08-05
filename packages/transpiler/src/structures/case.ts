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
    let ret = "let " + u + " = " + traversal.traverse(s).getCode() + ";\n";

    for (const w of node.findDirectStructures(abaplint.Structures.When)) {
      for (const c of w.getChildren()) {
        if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.When) {
          if (first === true) {
            first = false;
            ret += "if (";
          } else {
            ret += "} else if (";
          }
          ret += new WhenTranspiler(u).transpile(c, traversal).getCode() + ") {\n";
        } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.WhenOthers) {
          ret += "} else {\n";
        } else {
          ret += traversal.traverse(c).getCode(); // Normal
        }
      }
    }

    return new Chunk(ret + (first === true ? "" : "}\n"));
  }

}