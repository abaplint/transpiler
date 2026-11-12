import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class NewLineTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    // NEW-LINE [NO-SCROLLING | SCROLLING]: the next output starts a new line, scrolling has no effect on the text
    return new Chunk().append(traversal.beforeListOutput() + "abap.statements.newLine();", node, traversal);
  }

}
