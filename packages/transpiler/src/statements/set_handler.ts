import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodSourceTranspiler, SourceTranspiler} from "../expressions";

export class SetHandlerTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const methods: string[] = [];
    for (const m of node.findDirectExpressions(abaplint.Expressions.MethodSource)) {
      methods.push(new MethodSourceTranspiler().transpile(m, traversal).getCode());
    }

    let f: string | undefined = undefined;
    const forExpression = node.findExpressionAfterToken("FOR");
    if (forExpression) {
      f = new SourceTranspiler().transpile(forExpression, traversal).getCode();
    }

    let activation: string | undefined = undefined;
    const activationExpression = node.findExpressionAfterToken("ACTIVATION");
    if (activationExpression) {
      activation = new SourceTranspiler().transpile(activationExpression, traversal).getCode();
    }

    return new Chunk().append(`abap.statements.setHandler([${methods.join(",")}], ${f}, ${activation});`, node, traversal);
  }

}