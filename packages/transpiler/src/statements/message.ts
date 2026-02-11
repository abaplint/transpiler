import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MessageTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];

    const into = node.findExpressionAfterToken("INTO");
    if (into) {
      options.push("into: " + traversal.traverse(into).getCode());
    }

    const messagesource = node.findDirectExpression(abaplint.Expressions.MessageSource);
    if (messagesource) {
      const id = messagesource.findExpressionAfterToken("ID");
      if (id) {
        options.push("id: " + traversal.traverse(id).getCode());
      }

      const cla = messagesource.findDirectExpression(abaplint.Expressions.MessageClass);
      if (cla) {
        options.push("id: \"" + cla.concatTokens() + "\"");
      }

      const type = messagesource.findExpressionAfterToken("TYPE");
      if (type) {
        options.push("type: " + traversal.traverse(type).getCode());
      }

      const number = messagesource.findExpressionAfterToken("NUMBER");
      if (number) {
        options.push("number: " + traversal.traverse(number).getCode());
      }

      const typeAndNumber = messagesource.findDirectExpression(abaplint.Expressions.MessageTypeAndNumber);
      if (typeAndNumber) {
        const str = typeAndNumber.getFirstToken().getStr();
        options.push("number: \"" + str.substr(1, 3) + "\"");
        options.push("type: \"" + str.substr(0, 1).toUpperCase() + "\"");
      }

      const like = messagesource.findExpressionAfterToken("LIKE");
      if (like) {
        options.push("displayLike: " + traversal.traverse(like).getCode());
      }
    } else {
// exception or constant based
      const source = node.findDirectExpression(abaplint.Expressions.MessageSourceSource
        )?.findDirectExpression(abaplint.Expressions.Source);
      if (source) {
        options.push("exceptionOrText: " + traversal.traverse(source).getCode());
      }

      const type = node.findExpressionAfterToken("TYPE");
      if (type) {
        options.push("type: " + traversal.traverse(type).getCode());
      }

      const like = node.findExpressionAfterToken("LIKE");
      if (like) {
        options.push("displayLike: " + traversal.traverse(like).getCode());
      }
    }

    const w: string[] = [];
    let withs = false;
    for (const c of node.getChildren()) {
      if (c.getFirstToken().getStr().toUpperCase() === "WITH") {
        withs = true;
      } else if (withs === true
          && c.get() instanceof abaplint.Expressions.MessageSourceSource
          && c instanceof abaplint.Nodes.ExpressionNode) {
        w.push(traversal.traverse(c.getFirstChild()).getCode());
      } else if (withs === true) {
        break;
      }
    }
    if (w.length > 0) {
      options.push("with: [" + w.join(",") + "]");
    }

    return new Chunk()
      .append("await abap.statements.message({", node, traversal)
      .appendString(options.join(", "))
      .append("});", node.getLastToken(), traversal);
  }

}