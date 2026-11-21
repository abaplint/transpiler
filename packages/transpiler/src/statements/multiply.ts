import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {compoundResult} from "./_integer_result";

export class MultiplyTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const sourceExpression = node.findDirectExpression(abaplint.Expressions.Source);
    const source = traversal.traverse(sourceExpression);

    const operation = new Chunk();
    operation.append("abap.operators.multiply(", node, traversal);
    operation.appendChunk(target);
    operation.append(",", node, traversal);
    operation.appendChunk(source);
    operation.appendString(")");

    const ret = new Chunk();
    ret.appendChunk(target);
    ret.appendString(".set(");
    ret.appendChunk(compoundResult(node, sourceExpression, operation, traversal));
    ret.append(");", node.getLastToken(), traversal);
    return ret;
  }

}
