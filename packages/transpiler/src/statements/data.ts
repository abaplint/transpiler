import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../types";
import {Traversal} from "../traversal";
import {ConstantTranspiler} from "../expressions/constant";
import {FieldChainTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class DataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.DefinitionName)?.getFirstToken();
    if (token === undefined) {
      throw new Error("DataTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("DataTranspiler, scope not found");
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("DataTranspiler, var not found, \"" + token.getStr() + "\"");
    }

    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      let int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int === undefined) {
        int = val.findFirstExpression(abaplint.Expressions.ConstantString);
      }
      if (int) {
        const escaped = new ConstantTranspiler().escape(int.getFirstToken().getStr());
        value = "\n" + found.getName() + ".set(" + escaped + ");";
      } else if (val.getChildren()[1].get() instanceof abaplint.Expressions.SimpleFieldChain) {
        const s = new FieldChainTranspiler().transpile(val.getChildren()[1] as abaplint.Nodes.ExpressionNode, traversal).getCode();
        value = "\n" + found.getName() + ".set(" + s + ");";
      }
    }

    return new Chunk(new TranspileTypes().declare(found) + value);
  }

}