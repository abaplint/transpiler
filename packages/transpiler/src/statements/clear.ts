import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClearTranspiler implements IStatementTranspiler {
  private readonly body: boolean;

  /** body: FREE, which clears the body of a table with a header line; CLEAR clears its header line */
  public constructor(body = false) {
    this.body = body;
  }

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
    if (this.body === false && targetNode && ClearTranspiler.isHeaderLineTable(targetNode, traversal)) {
      // CLEAR itab of a table with a header line clears the header line, CLEAR itab[] the rows
      ret.append(".getHeader().clear();", node.getLastToken(), traversal);
    } else {
      ret.append(".clear();", node.getLastToken(), traversal);
    }
    return ret;
  }

  /** the target is the plain name of a table with a header line (not itab[]) */
  private static isHeaderLineTable(target: abaplint.Nodes.ExpressionNode, traversal: Traversal): boolean {
    if (target.getChildren().length !== 1 || !(target.getFirstChild()?.get() instanceof abaplint.Expressions.TargetField)) {
      return false;
    }
    const name = target.getFirstToken();
    const type = traversal.findCurrentScopeByToken(name)?.findVariable(name.getStr().replace(/^!/, ""))?.getType();
    return type instanceof abaplint.BasicTypes.TableType && type.isWithHeader();
  }

}
