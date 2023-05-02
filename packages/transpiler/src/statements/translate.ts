import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {SourceTranspiler} from "../expressions/index.js";
import {Chunk} from "../chunk.js";

export class TranslateTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    let type = "";
    if (node.findDirectTokenByText("UPPER")) {
      type = `"UPPER"`;
    } else if (node.findDirectTokenByText("LOWER")) {
      type = `"LOWER"`;
    } else {
      const s = node.findDirectExpression(abaplint.Expressions.Source);
      if (s) {
        type = new SourceTranspiler(true).transpile(s, traversal).getCode();
      } else {
        throw new Error("TranslateTranspiler, Source expression not found");
      }
    }

    return new Chunk().append("abap.statements.translate(" + target + ", " + type + ");", node, traversal);
  }

}