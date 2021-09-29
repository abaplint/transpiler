import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ReturnTranspiler} from ".";

export class CheckTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond));

    const ret = new Chunk();
    ret.append("if (!(", node, traversal);
    ret.appendChunk(cond);
    ret.appendString(")) {\n");
    if (this.isInsideLoop(node, traversal)) {
      ret.appendString("continue;");
    } else {
      ret.appendChunk(new ReturnTranspiler().transpile(node, traversal));
    }
    ret.append("\n}", node.getLastToken(), traversal);
    return ret;
  }

  private isInsideLoop(node: abaplint.Nodes.StatementNode, traversal: Traversal): boolean {
    const stack: abaplint.Nodes.StatementNode[] = [];

    for (const statement of traversal.getFile().getStatements()) {
      const get = statement.get();
      if (get instanceof abaplint.Statements.Loop
          || get instanceof abaplint.Statements.While
          || get instanceof abaplint.Statements.SelectLoop
          || get instanceof abaplint.Statements.Do) {
        stack.push(statement);
      } else if (get instanceof abaplint.Statements.EndLoop
          || get instanceof abaplint.Statements.EndWhile
          || get instanceof abaplint.Statements.EndSelect
          || get instanceof abaplint.Statements.EndDo) {
        stack.pop();
      }
      if (statement === node) {
        break;
      }
    }

    return stack.length > 0;
  }

}