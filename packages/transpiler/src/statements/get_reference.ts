import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class GetReferenceTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const s = node.findDirectExpression(abaplint.Expressions.Source);
    const t = node.findDirectExpression(abaplint.Expressions.Target);
    if (s === undefined) {
      throw new Error("GetReference, Source not found");
    } else if (t === undefined) {
      throw new Error("GetReference, Target not found");
    }

    let source = traversal.traverse(s).getCode();
    const target = traversal.traverse(t).getCode();

    if (s.getFirstToken().getStr().startsWith("<")) {
      // its a field symbol
      source += ".getPointer()";
    }

    return new Chunk(target + ".assign(" + source + ");");
  }

}