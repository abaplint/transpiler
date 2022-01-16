import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "../expressions";

export class CreateDataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const targetNode = node.findDirectExpression(abaplint.Expressions.Target);
    const target = traversal.traverse(targetNode);

    const options: string[] = [];

    let dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.ConstantString);
    if (dynamic) {
      options.push(`"name": ` + dynamic.getFirstToken().getStr());
    } else {
      dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.FieldChain);
      if (dynamic) {
        options.push(`"name": ` + new FieldChainTranspiler(true).transpile(dynamic, traversal).getCode());
      }
    }

    if (node.findDirectTokenByText("TABLE")) {
      options.push(`"table": true`);
    }

    let add = "";
    if (options.length > 0) {
      add = ",{" + options.join(",") + "}";
    }

    return new Chunk("abap.statements.createData(" + target.getCode() + add + ");");
  }

}