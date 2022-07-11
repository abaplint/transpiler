import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "../expressions";
import {TranspileTypes} from "../transpile_types";

export class CreateDataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const targetNode = node.findDirectExpression(abaplint.Expressions.Target);
    const target = traversal.traverse(targetNode);
    const concat = node.concatTokens().toUpperCase();
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

    const typeNameNode = node.findDirectExpression(abaplint.Expressions.TypeName);
    if (typeNameNode) {
      const id = traversal.findCurrentScopeByToken(typeNameNode.getFirstToken())?.findType(typeNameNode.concatTokens());
      if (id) {
        options.push(`"type": ` + new TranspileTypes().toType(id.getType()));
      }
    }

    if (node.findDirectTokenByText("TABLE")) {
      options.push(`"table": true`);
    }

    if (concat.includes(" LIKE LINE OF ")) {
      const so = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
      options.push(`"likeLineOf": ` + so.getCode());
    } else if (concat.includes(" LIKE ")) {
      const so = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
      options.push(`"like": ` + so.getCode());
    }

    const handle = node.findExpressionAfterToken("HANDLE");
    if (handle) {
      const so = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
      options.push(`"typeHandle": ` + so.getCode());
    }

    let add = "";
    if (options.length > 0) {
      add = ",{" + options.join(",") + "}";
    }

    return new Chunk("abap.statements.createData(" + target.getCode() + add + ");");
  }

}