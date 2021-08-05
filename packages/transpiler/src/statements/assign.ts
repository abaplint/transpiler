import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AssignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source).map(
      e => new SourceTranspiler(false).transpile(e, traversal).getCode());
    const fs = new FieldSymbolTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.FSTarget)!, traversal).getCode();

    const options: string[] = [];

    const concat = node.concatTokens();
    if (concat.startsWith("ASSIGN COMPONENT ")) {
      options.push("component: " + sources.shift());
    }

    options.push("target: " + fs);
    options.push("source: " + sources.pop());

    if (concat.endsWith(" CASTING.")) {
      options.push("casting: true");
    }

    return new Chunk("abap.statements.assign({" + options.join(", ") + "});");
  }

}