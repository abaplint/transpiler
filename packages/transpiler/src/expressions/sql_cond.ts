import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SimpleSource3Transpiler} from "./simple_source3";
import {FieldChainTranspiler} from "./field_chain";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode
          && c.get() instanceof abaplint.Expressions.SQLCompare) {
        if (ret !== "") {
          ret += " ";
        }
        if (c.findDirectExpression(abaplint.Expressions.Dynamic)) {
          const chain = c.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.FieldChain);
          if (chain) {
            const code = new FieldChainTranspiler(true).transpile(chain, traversal).getCode();
            ret += `" + abap.expandDynamic(${code}, (name) => {try { return eval(name);} catch {}}) + "`;
          } else {
            throw new Error("SQL Condition, transpiler todo, dyn cond, " + c.concatTokens());
          }
        } else if (c.findDirectExpression(abaplint.Expressions.SQLIn)) {
          ret += this.sqlIn(c, traversal);
        } else {
          ret += this.basicCondition(c, traversal, traversal.getFilename());
        }
      } else if (c instanceof abaplint.Nodes.ExpressionNode) {
        ret += " " + this.transpile(c, traversal).getCode();
      } else {
        ret += " " + c.concatTokens();
      }
    }

    const c = new Chunk();
    return c.appendString(ret.trim());
  }

  private sqlIn(c: abaplint.Nodes.ExpressionNode, _traversal: Traversal): string {
    const fieldName = c.findDirectExpression(abaplint.Expressions.SQLFieldName);
    const slqin = c.findDirectExpression(abaplint.Expressions.SQLIn);
    const source = c.findFirstExpression(abaplint.Expressions.SimpleSource3);
    if (fieldName === undefined || slqin === undefined || source === undefined) {
      throw new Error("SQL Condition, transpiler todo, " + c.concatTokens());
    }

    const ret = `" + abap.expandIN("${fieldName.concatTokens()}", ${source.concatTokens()}) + "`;

    return ret;
  }

  private basicCondition(c: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string): string {
    let ret = "";
    if (c.getChildren().length !== 3) {
      return this.basicConditionNew(c, traversal, filename);
    }

    let fieldName: string | undefined = undefined;
    const fieldNameExpression = c.findDirectExpression(abaplint.Expressions.SQLFieldName);
    if (fieldNameExpression) {
      fieldName = new SQLFieldNameTranspiler().transpile(fieldNameExpression, traversal).getCode();
    }
    const operator = c.findDirectExpression(abaplint.Expressions.SQLCompareOperator);
    const source = c.findDirectExpression(abaplint.Expressions.SQLSource);

    if (fieldName && source && operator === undefined && c.findDirectTokenByText("LIKE")) {
      ret += fieldName + " LIKE ";
      ret += this.sqlSource(source, traversal, filename);
      return ret;
    }

    if (fieldName === undefined || operator === undefined || source === undefined) {
      throw new Error("SQL Condition, transpiler todo2, " + c.concatTokens());
    }

    let op = operator.concatTokens();
    if (op.toUpperCase() === "EQ") {
      op = "=";
    } else if (op.toUpperCase() === "NE") {
      op = "<>";
    }
    ret += fieldName + " " + op + " ";
    ret += this.sqlSource(source, traversal, filename);

    return ret;
  }

  private sqlSource(source: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string) {
    let ret = "";
    const simple = source.findDirectExpression(abaplint.Expressions.SimpleSource3);
    const alias = source.findDirectExpression(abaplint.Expressions.SQLAliasField);
    if (simple && simple.findDirectExpression(abaplint.Expressions.Constant) === undefined) {
      ret += "'\" + " + new SimpleSource3Transpiler(true).transpile(simple, traversal).getCode() + " + \"'";
    } else if (alias) {
      // SQLAliasField might be a SQL reference or value from ABAP interface
      const pre = alias.concatTokens().split("~")[0];
      const found = traversal.findInterfaceDefinition(pre, traversal.findCurrentScopeByToken(alias.getFirstToken()));
      if (found) {
        let name = traversal.prefixAndName(alias.getFirstToken(), filename).replace("~", "$");
        name = Traversal.escapeNamespace(name)!;
        ret += "'\" + " + name + ".get() + \"'";
      } else {
        ret += source.concatTokens();
      }
    } else {
      const concat = source.concatTokens();
      if (concat.startsWith("`")) {
        ret += "'" + concat.substring(1, concat.length - 1) + "'";
      } else {
        ret += concat;
      }
    }
    return ret;
  }

  private basicConditionNew(node: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string): string {
    let ret = "";
    for (const child of node.getChildren()) {
      if (ret !== "") {
        ret += " ";
      }
      if (child.get() instanceof abaplint.Expressions.SQLFieldName
          && child instanceof abaplint.Nodes.ExpressionNode) {
        ret += new SQLFieldNameTranspiler().transpile(child, traversal).getCode();
      } else if (child.get() instanceof abaplint.Expressions.SQLSource
          && child instanceof abaplint.Nodes.ExpressionNode) {
        ret += this.sqlSource(child, traversal, filename);
      } else {
        ret += child.concatTokens();
      }
    }
    return ret;
  }

}