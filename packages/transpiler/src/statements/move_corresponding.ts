import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TargetTranspiler} from "../expressions";

export class MoveCorrespondingTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.SimpleTarget)!, traversal);
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    const concat = node.concatTokens().toUpperCase();
    const options: string[] = [];
    if (concat.includes(" KEEPING TARGET LINES")) {
      options.push("keepingTargetLines: true");
    }
    if (concat.includes(" EXPANDING NESTED TABLES")) {
      options.push("expandingNestedTables: true");
    }
    const extra = options.length > 0 ? ", {" + options.join(", ") + "}" : "";

    return new Chunk().append("abap.statements.moveCorresponding(", node, traversal)
      .join([source, target])
      .append(extra + ");", node.getLastToken(), traversal);
  }

}