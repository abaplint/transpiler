import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

// GENERATE SUBROUTINE POOL cannot be supported, there is no compiler at runtime.
// It is refused the way a system reports a pool it could not generate, no exception:
// sy-subrc = 8 ("other generation error"), NAME initial, MESSAGE filled, LINE and WORD
// initial. The other additions are left untouched.
export class GenerateSubroutineTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const name = this.findAddition(node, "NAME");
    if (name) {
      ret.appendString(traversal.traverse(name).getCode() + ".clear();\n");
    }
    const message = this.findAddition(node, "MESSAGE");
    if (message) {
      ret.appendString(traversal.traverse(message).getCode() + `.set("GENERATE SUBROUTINE POOL is not supported");\n`);
    }
    const line = this.findAddition(node, "LINE");
    if (line) {
      ret.appendString(traversal.traverse(line).getCode() + ".set(0);\n");
    }
    const word = this.findAddition(node, "WORD");
    if (word) {
      ret.appendString(traversal.traverse(word).getCode() + ".clear();\n");
    }

    ret.append("abap.builtin.sy.get().subrc.set(8);", node, traversal);
    return ret;
  }

  /** the expression directly after the addition keyword, MESSAGE does not match MESSAGE-ID */
  private findAddition(node: abaplint.Nodes.StatementNode, keyword: string): abaplint.Nodes.ExpressionNode | undefined {
    const children = node.getChildren();
    for (let i = 0; i < children.length - 1; i++) {
      const child = children[i];
      const next = children[i + 1];
      if (child instanceof abaplint.Nodes.TokenNode
          && child.getFirstToken().getStr().toUpperCase() === keyword
          && next instanceof abaplint.Nodes.ExpressionNode) {
        return next;
      }
    }
    return undefined;
  }

}
