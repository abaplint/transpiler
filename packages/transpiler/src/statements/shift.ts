import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ShiftTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    const options: string[] = [];

    if (node.findDirectTokenByText("LEFT")) {
      options.push("direction: 'LEFT'");
    } else if (node.findDirectTokenByText("RIGHT")) {
      options.push("direction: 'RIGHT'");
    }

    if (node.findDirectTokenByText("CIRCULAR")) {
      options.push("circular: true");
    }

    const leading = node.findExpressionAfterToken("LEADING");
    if (leading) {
      options.push("deletingLeading: " + traversal.traverse(leading).getCode());
    }
    const trailing = node.findExpressionAfterToken("TRAILING");
    if (trailing) {
      options.push("deletingTrailing: " + traversal.traverse(trailing).getCode());
    }

    const places = node.findExpressionAfterToken("BY");
    if (places) {
      options.push("places: " + traversal.traverse(places).getCode());
    }

    const to = node.findExpressionAfterToken("TO");
    if (to) {
      options.push("to: " + traversal.traverse(to).getCode());
    }

    if (node.findDirectTokenByText("BYTE")) {
      options.push("mode: 'BYTE'");
    }
    const extra = options.length > 0 ? ", {" + options.join(",") + "}" : "";
    return new Chunk().append("abap.statements.shift(" + target + extra + ");", node, traversal);
  }

}