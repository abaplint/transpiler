import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../transpile_types";
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
      throw new Error("DataTranspiler, scope not found: " + node.concatTokens());
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("DataTranspiler, var not found, \"" + token.getStr() + "\", " + traversal.getFilename() + ", line: " + token.getRow());
    }

    let value = "";
    if (found.getValue() !== undefined && node.concatTokens().includes(" & ")) {
      value = "\n" + traversal.setValues(found, found.getName());
    } else {
      value = DataTranspiler.buildValue(node, found.getName().toLowerCase(), traversal);
    }

    const ret = new Chunk()
      .appendString("let ")
      .append(found.getName().toLowerCase(), token, traversal)
      .appendString(" = " + new TranspileTypes().toType(found.getType()))
      .append(";", node.getLastToken(), traversal)
      .append(value, node.getLastToken(), traversal);

    return ret;
  }

  public static buildValue(node: abaplint.Nodes.StatementNode, name: string, traversal: Traversal): string {
    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      let int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int === undefined) {
        int = val.findFirstExpression(abaplint.Expressions.ConstantString);
      }
      if (int) {
        const escaped = ConstantTranspiler.escape(int.concatTokens());
        value = "\n" + name + ".set(" + escaped + ");";
      } else if (val.getChildren()[1].get() instanceof abaplint.Expressions.SimpleFieldChain) {
        const s = new FieldChainTranspiler().transpile(val.getChildren()[1] as abaplint.Nodes.ExpressionNode, traversal).getCode();
        value = "\n" + name + ".set(" + s + ");";
      }
    }
    return value;
  }

}