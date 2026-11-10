import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClearTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const targetNode = node.findDirectExpression(abaplint.Expressions.Target);
    const target = traversal.traverse(targetNode);

    // Reference implementation for the source-map convention:
    // - keep the target's own expression-level mappings by appending its Chunk
    //   (do NOT flatten with target.getCode(), that discards the mappings)
    // - map the trailing generated syntax to the statement's last token, not
    //   getEnd() which points one column past the statement
    const ret = new Chunk();
    ret.appendChunk(target);
    // "CLEAR itab" of a table with header line clears the header line, "CLEAR itab[]" the body
    const clear = this.hasHeaderLine(targetNode, traversal) ? ".getHeader().clear();" : ".clear();";
    ret.append(clear, node.getLastToken(), traversal);
    return ret;
  }

  private hasHeaderLine(target: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): boolean {
    if (target === undefined || /^[\w\/]+$/.test(target.concatTokens()) === false) {
      return false;
    }
    const token = target.getFirstToken();
    const type = traversal.findCurrentScopeByToken(token)?.findVariable(target.concatTokens())?.getType();
    return type instanceof abaplint.BasicTypes.TableType && type.isWithHeader();
  }

}
