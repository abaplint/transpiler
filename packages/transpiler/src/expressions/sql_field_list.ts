import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldTranspiler} from "./sql_field";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLFieldListTranspiler implements IExpressionTranspiler {

  /**
   * byPosition: the rows are filled by position, but the database clients return
   * them keyed by column name, so two columns of one name (a~id b~id) come back as
   * one. Where a plain column repeats the name of another in the list, the entries
   * without an alias of their own get one, "#0", "#1", ... by position in the
   * list: a name no column or alias can have. A list without such a clash is
   * left exactly as it is.
   */
  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, byPosition = false): Chunk {

    const clashing = byPosition ? this.clashingNames(node) : new Set<string>();

    const fields: string[] = [];
    for (const f of node.getChildren() || []) {
      if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLField) {
        let code = new SQLFieldTranspiler().transpile(f, traversal).getCode();
        const column = this.plainColumn(f);
        if (column !== undefined && column.alias === false && clashing.has(column.name)) {
          code = code.trimEnd() + ` AS \\"#${fields.length}\\"`;
        }
        fields.push(code);
      } else if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLFieldName) {
        const code = new SQLFieldNameTranspiler().transpile(f, traversal).getCode();
        fields.push(code);
      } else if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLFieldList) {
        // todo, I have no idea why its nested like this when there is just one field
        return new SQLFieldListTranspiler().transpile(f, traversal, byPosition);
      } else {
        const concat = f.concatTokens();
        if (concat !== ",") {
          fields.push(concat);
        }
      }
    }

    return new Chunk().append(fields.join(", "), node, traversal);
  }

  // "col", "tab~col" or one of them with "AS name": the name its column has in the row
  private plainColumn(field: Nodes.ExpressionNode): {name: string, alias: boolean} | undefined {
    const children = field.getChildren();
    const first = children[0];
    if (!(first instanceof abaplint.Nodes.ExpressionNode) || !(first.get() instanceof abaplint.Expressions.SQLFieldName)) {
      return undefined;
    }
    const as = field.findDirectExpression(abaplint.Expressions.SQLAsName);
    if (children.length === 1) {
      return {name: first.concatTokens().split("~").pop()!.toLowerCase(), alias: false};
    } else if (children.length === 3 && as !== undefined && children[2] === as) {
      return {name: as.concatTokens().toLowerCase(), alias: true};
    }
    return undefined;
  }

  private clashingNames(node: Nodes.ExpressionNode): Set<string> {
    const seen = new Set<string>();
    const clashing = new Set<string>();
    for (const f of node.getChildren() || []) {
      if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLField) {
        const column = this.plainColumn(f);
        if (column === undefined) {
          continue;
        } else if (seen.has(column.name)) {
          clashing.add(column.name);
        }
        seen.add(column.name);
      }
    }
    return clashing;
  }

}