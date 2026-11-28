import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

/** SELECTION-SCREEN is screen layout, there is no screen, so nothing runs. The statement declares
 * a field for COMMENT, PUSHBUTTON and TAB, and a structure for BEGIN OF TABBED BLOCK, those are
 * declared as they can be referenced, eg. setting the text of a comment in INITIALIZATION */
export class SelectionScreenTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let token = node.findFirstExpression(abaplint.Expressions.InlineField)?.getFirstToken();
    if (token === undefined && node.concatTokens().toUpperCase().includes("BEGIN OF TABBED BLOCK")) {
      token = node.findFirstExpression(abaplint.Expressions.BlockName)?.getFirstToken();
    }
    if (token === undefined) {
      return new Chunk("");
    }

    const found = traversal.findCurrentScopeByToken(token)?.findVariable(token.getStr().replace(/^!/, ""));
    if (found === undefined) {
      return new Chunk("");
    }
    const name = Traversal.prefixVariable(Traversal.escapeNamespace(found.getName().toLowerCase()));
    return new Chunk("let " + name + " = " + traversal.toType(found.getType()) + ";");
  }

}
