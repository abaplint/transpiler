import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {UniqueIdentifier} from "../unique_identifier";
import {Chunk} from "../chunk";

export class CaseTypeTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    // does not use switch(), as it break;'s EXITs
    const s = node.findDirectStatement(abaplint.Statements.CaseType)?.findDirectExpression(abaplint.Expressions.Source);
    if (s === undefined) {
      throw new Error("CASE TYPE, no Source found");
    }

    let first = true;
    const u = UniqueIdentifier.get();
    const ret = new Chunk();

    // this determines and copies the value
    ret.append("let " + u + " = ", node, traversal);
    ret.appendChunk(traversal.traverse(s));
    ret.append(";\n", node, traversal);

    for (const w of node.findDirectStructures(abaplint.Structures.WhenType)) {
      for (const c of w.getChildren()) {
        if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.WhenType) {
          if (first === true) {
            first = false;
            ret.appendString("if (");
          } else {
            ret.appendString("} else if (");
          }
          const cname = c.findDirectExpression(abaplint.Expressions.ClassName);
          const lookup = traversal.lookupClassOrInterface(cname?.concatTokens(), cname?.getFirstToken());
          ret.appendString(u + ".get() instanceof " + lookup);
          ret.appendString(") {\n");
          ret.appendString(traversal.traverse(c.findDirectExpression(abaplint.Expressions.Target)).getCode() + ".set(" + u + ")\n");
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