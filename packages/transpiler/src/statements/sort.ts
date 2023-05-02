import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SortTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();
    const components = node.findDirectExpressions(abaplint.Expressions.ComponentChain);
    const options: string[] = [];

    if (concat.includes(" BY ") === false) {
      const descending = node.findDirectTokenByText("DESCENDING") !== undefined;
      if (descending === true) {
        options.push("descending: true");
      }
    } else if (components.length > 0) {
      const by: string[] = [];
      for (const c of components) {
        const next = this.findNextText(c, node);
        if (next === "DESCENDING") {
          by.push(`{component: "${c.concatTokens().toLowerCase()}", descending: true}`);
        } else {
          by.push(`{component: "${c.concatTokens().toLowerCase()}"}`);
        }
      }
      options.push(`by: [${by.join(",")}]`);
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    return new Chunk().append("abap.statements.sort(" + target + ",{" + options.join(",") + "});", node, traversal);
  }

  private findNextText(c: abaplint.Nodes.ExpressionNode, parent: abaplint.Nodes.StatementNode): string {
    const children = parent.getChildren();
    for (let i = 0; i < children.length; i++) {
      const element = children[i];
      if (element !== c) {
        continue;
      }
      const next = children[i + 1];
      if (next) {
        return next.concatTokens().toUpperCase();
      }
    }
    return "";
  }

}