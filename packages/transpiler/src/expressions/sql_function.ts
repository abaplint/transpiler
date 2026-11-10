import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";
import {SimpleSource3Transpiler} from "./simple_source3";
import {SourceTranspiler} from "./source";

/** SQL function call like substring( field, 8, 1 ), passed through by name */
export class SQLFunctionTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const name = node.getFirstToken().getStr().toLowerCase();
    const inputs = node.findDirectExpressions(abaplint.Expressions.SQLFunctionInput)
      .map(i => this.input(i, traversal));
    return new Chunk().appendString(name + "(" + inputs.join(", ") + ")");
  }

  private input(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const ret: string[] = [];
    let host = false;
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        if (c.getFirstToken().getStr() === "@") {
          host = true;
        } else {
          ret.push(c.concatTokens());
        }
        continue;
      }

      const get = c.get();
      if (host && get instanceof abaplint.Expressions.SimpleSource3) {
        const code = new SimpleSource3Transpiler(true).transpile(c, traversal).getCode();
        ret.push(`'" + abap.sqlEscape(${code}) + "'`);
        host = false;
      } else if (host && get instanceof abaplint.Expressions.Source) {
        const code = new SourceTranspiler(true).transpile(c, traversal).getCode();
        ret.push(`'" + abap.sqlEscape(${code}) + "'`);
        host = false;
      } else if (get instanceof abaplint.Expressions.SQLFieldName && /^\d+$/.test(c.concatTokens())) {
        // integer literals are parsed as field names
        ret.push(c.concatTokens());
      } else if (get instanceof abaplint.Expressions.SQLFieldName || get instanceof abaplint.Expressions.SQLAliasField) {
        ret.push(new SQLFieldNameTranspiler().transpile(c, traversal).getCode());
      } else if (get instanceof abaplint.Expressions.SQLFunction) {
        ret.push(this.transpile(c, traversal).getCode());
      } else if (get instanceof abaplint.Expressions.SQLFunctionInput) {
        ret.push(this.input(c, traversal));
      } else {
        const concat = c.concatTokens();
        if (concat.startsWith("`")) {
          ret.push("'" + concat.substring(1, concat.length - 1).replace(/'/g, "''") + "'");
        } else {
          ret.push(concat);
        }
      }
    }
    return ret.join(" ").replace(/\( /g, "(").replace(/ \)/g, ")");
  }

}
