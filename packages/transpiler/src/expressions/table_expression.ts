import {Nodes, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ComponentChainSimpleTranspiler} from "./component_chain_simple";
import {UniqueIdentifier} from "../unique_identifier";

export class TableExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, source: Chunk): Chunk {
    if (source === undefined) {
      throw new Error("TableExpressionTranspiler: Source chunk is undefined");
    }

    const ret = new Chunk();
    const extra: string[] = [];
    let field = "";
    let usesTableLine = false;
    const withKey: string[] = [];
    const withKeyValue: string[] = [];
    const values: string[] = [];

    // KEY name: the row is searched, or counted by INDEX, in the order of that key
    const keyName = node.findDirectExpression(Expressions.SimpleName);
    if (keyName) {
      extra.push(`keyName: "${keyName.concatTokens().toLowerCase()}"`);
    }

    if (node.findDirectExpression(Expressions.Dynamic)) {
      return ret.appendString(`(function() { throw new Error("Table expression with a dynamic key, not supported, transpiler") })()`);
    } else if (node.getChildren().length === 3 && node.findDirectExpression(Expressions.Source)) {
      extra.push(`index: ${traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode()}`);
    } else if (node.findDirectTokenByText("INDEX")) {
      extra.push(`index: ${traversal.traverse(node.findExpressionAfterToken("INDEX")).getCode()}`);
    } else {
      // conditions
      for (const c of node.getChildren()) {
        // only the keywords, a component can be named "key" or "components"
        if (c === keyName || (c instanceof Nodes.TokenNode && ["KEY", "COMPONENTS"].includes(c.concatTokens().toUpperCase()))) {
          continue;
        }
        if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ComponentChainSimple) {
          field = new ComponentChainSimpleTranspiler("i.").transpile(c, traversal).getCode();
          if (field === "i.table_line") {
            usesTableLine = true;
          }
        } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Source) {
          // the value is evaluated once, before the search
          const id = UniqueIdentifier.get();
          values.push("const " + id + " = " + traversal.traverse(c).getCode() + ";");
          withKey.push("abap.compare.eq(" + field + ", " + id + ")");
          withKeyValue.push(`{key: (i) => {return ${field}}, value: ${id}}`);
        } else if (c.concatTokens() === "=" || c.concatTokens() === "[" || c.concatTokens() === "]") {
          continue;
        } else {
          throw new Error("TableExpressionTranspiler: todo, other, " + c.concatTokens());
        }
      }

      extra.push("withKey: async (i) => {return " + withKey.join(" && ") + ";}");
      extra.push(`withKeyValue: [${withKeyValue.join(",")}]`);
      extra.push("usesTableLine: " + usesTableLine);
    }

    const call = `abap.operators.tableExpression(${source.getCode()}, {${extra.join(", ")} })`;
    if (values.length > 0) {
      ret.appendString(`(await (async () => {${values.join(" ")} return ${call};})())`);
    } else {
      ret.appendString(`(await ${call})`);
    }

    return ret;
  }

}