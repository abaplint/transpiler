import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SimpleSource3Transpiler} from "./simple_source3";
import {FieldChainTranspiler} from "./field_chain";
import {SQLFieldNameTranspiler} from "./sql_field_name";
import {TranspileTypes} from "../transpile_types";
import {SourceTranspiler} from "./source";

export class SQLCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, table?: abaplint.Objects.Table | undefined): Chunk {
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
            const code = new FieldChainTranspiler(false).transpile(chain, traversal).getCode();
            ret += `" + abap.expandDynamic(${code}, (name) => {try { return eval(name);} catch {}}) + "`;
          } else {
            const concat = c.concatTokens();
            if (concat.toUpperCase() === "('ENABLE_SQLITE = ABAP_TRUE')") {
              // yea, this is a workaround
              ret += ` \\"enable_sqlite\\" = 'X'`;
            } else {
              throw new Error("SQL Condition, transpiler todo, dyn cond, " + concat + ", " + traversal.getFilename());
            }
          }
        } else if (c.findDirectExpression(abaplint.Expressions.SQLIn)) {
          ret += this.sqlIn(c, traversal, traversal.getFilename(), table);
        } else {
          ret += this.basicCondition(c, traversal, traversal.getFilename(), table);
        }
      } else if (c instanceof abaplint.Nodes.ExpressionNode) {
        ret += " " + this.transpile(c, traversal, table).getCode();
      } else {
        ret += " " + c.concatTokens();
      }
    }

    const c = new Chunk();
    return c.appendString(ret.trim());
  }

  private sqlIn(c: abaplint.Nodes.ExpressionNode, traversal: Traversal,
                filename: string, table: abaplint.Objects.Table | undefined): string {
    const fieldName = c.findDirectExpression(abaplint.Expressions.SQLFieldName)
      || c.findDirectExpression(abaplint.Expressions.SQLAliasField);
    const sqlin = c.findDirectExpression(abaplint.Expressions.SQLIn);
    const source = c.findFirstExpression(abaplint.Expressions.SimpleSource3);

    if (fieldName === undefined || sqlin === undefined || source === undefined) {
      throw new Error("SQL Condition, transpiler todo, " + c.concatTokens());
    }

    let pre = "";
    if (c.concatTokens().toUpperCase().includes(" NOT IN ")) {
      pre = "NOT ";
    }

    if (sqlin.getChildren().length === 2) {
      const s = new SourceTranspiler().transpile(source, traversal).getCode();
      return `${pre}" + abap.expandIN("${fieldName.concatTokens()}", ${s}) + "`;
    } else {
      const cond: string[] = [];
      for (const s of sqlin.findDirectExpressions(abaplint.Expressions.SQLSource).concat(
          sqlin.findDirectExpressions(abaplint.Expressions.SQLSourceNoSpace))) {
        const field = new SQLFieldNameTranspiler().transpile(fieldName, traversal).getCode();
        const sourc = this.sqlSource(s, traversal, filename, table);
        cond.push(field + " = " + sourc);
      }
      const ret = pre + "( " + cond.join(" OR ") + " )";
      return ret;
    }
  }

  private basicCondition(c: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string,
                         table: abaplint.Objects.Table | undefined): string {
    let ret = "";
    if (c.getChildren().length !== 3) {
      return this.basicConditionNew(c, traversal, filename, table);
    }

    let fieldName: string | undefined = undefined;
    const fieldNameExpression = c.findDirectExpression(abaplint.Expressions.SQLFieldName)
      || c.findDirectExpression(abaplint.Expressions.SQLAliasField);
    if (fieldNameExpression) {
      fieldName = new SQLFieldNameTranspiler().transpile(fieldNameExpression, traversal).getCode();
    }
    const operator = c.findDirectExpression(abaplint.Expressions.SQLCompareOperator);
    const source = c.findDirectExpression(abaplint.Expressions.SQLSource);

    if (fieldName && source && operator === undefined && c.findDirectTokenByText("LIKE")) {
      ret += fieldName + " LIKE ";
      ret += this.sqlSource(source, traversal, filename, table);
      return ret;
    }

    if (fieldNameExpression && c.getChildren().length === 3 && c.concatTokens().toUpperCase().endsWith(" IS NULL")) {
      ret += fieldName + " IS NULL";
      return ret;
    }

    if (fieldName === undefined || operator === undefined || source === undefined) {
      throw new Error("SQL Condition, transpiler todo2, " + c.concatTokens());
    }

    const op = this.sqlOperator(operator.concatTokens());
    ret += fieldName + " " + op + " ";
    ret += this.sqlSource(source, traversal, filename, table);

    return ret;
  }

  private sqlSource(source: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string,
                    table: abaplint.Objects.Table | undefined) {
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
        let concat = source.concatTokens();
        if (concat.includes("~") && concat.split("~")[0].includes("/")) {
          concat = "'" + concat.replace("~", "'~");
        }
        ret += concat;
      }
    } else {
      const concat = source.concatTokens();
      const conversionField = traversal.isSQLConversion(source.getFirstToken());
      if (conversionField) {
        const field = (table?.parseType(traversal.reg) as abaplint.BasicTypes.StructureType).getComponentByName(conversionField);
        ret += "'\" + " + TranspileTypes.toType(field!) + ".set(" + concat + ").get() + \"'";
      } else if (concat.startsWith("`")) {
        ret += "'" + concat.substring(1, concat.length - 1) + "'";
      } else {
        ret += concat;
      }
    }
    return ret;
  }

  private basicConditionNew(node: abaplint.Nodes.ExpressionNode, traversal: Traversal, filename: string,
                            table: abaplint.Objects.Table | undefined): string {
    const sourceWithComponents = this.sourceWithComponents(node, traversal);
    if (sourceWithComponents) {
      return sourceWithComponents;
    }

    let ret = "";
    for (const child of node.getChildren()) {
      if (ret !== "") {
        ret += " ";
      }
      if ((child.get() instanceof abaplint.Expressions.SQLFieldName
          || child.get() instanceof abaplint.Expressions.SQLAliasField)
          && child instanceof abaplint.Nodes.ExpressionNode) {
        ret += new SQLFieldNameTranspiler().transpile(child, traversal).getCode();
      } else if (child.get() instanceof abaplint.Expressions.SQLSource
          && child instanceof abaplint.Nodes.ExpressionNode) {
        ret += this.sqlSource(child, traversal, filename, table);
      } else if (child.get() instanceof abaplint.Expressions.SQLCompareOperator
          && child instanceof abaplint.Nodes.ExpressionNode) {
        ret += this.sqlOperator(child.concatTokens());
      } else {
        ret += child.concatTokens();
      }
    }
    return ret;
  }

  private sourceWithComponents(node: abaplint.Nodes.ExpressionNode, traversal: Traversal): string | undefined {
    const children = node.getChildren();
    const fieldName = children[0];
    const operator = children[1];
    const source = children[2];
    if (!(fieldName instanceof abaplint.Nodes.ExpressionNode)
        || !(fieldName.get() instanceof abaplint.Expressions.SQLFieldName)
        || !(operator instanceof abaplint.Nodes.ExpressionNode)
        || !(operator.get() instanceof abaplint.Expressions.SQLCompareOperator)
        || !(source instanceof abaplint.Nodes.ExpressionNode)
        || !(source.get() instanceof abaplint.Expressions.SQLSource)
        || children.length < 5
        || (children.length - 3) % 2 !== 0) {
      return undefined;
    }

    const components: string[] = [];
    for (let i = 3; i < children.length; i += 2) {
      const dash = children[i];
      const component = children[i + 1];
      if (!(dash instanceof abaplint.Nodes.TokenNode)
          || dash.getFirstToken().getStr() !== "-"
          || !(component instanceof abaplint.Nodes.ExpressionNode)
          || !(component.get() instanceof abaplint.Expressions.SQLFieldName)) {
        return undefined;
      }
      components.push(component.concatTokens().toLowerCase());
    }

    const simple = source.findDirectExpression(abaplint.Expressions.SimpleSource3);
    if (simple === undefined || simple.findDirectExpression(abaplint.Expressions.Constant)) {
      return undefined;
    }

    let code = new SimpleSource3Transpiler(false).transpile(simple, traversal).getCode();
    for (const component of components) {
      if (component.match(/^\d/) || component.includes("/")) {
        code += `.get()["${component}"]`;
      } else {
        code += `.get().${component}`;
      }
    }
    code += ".get()";

    const field = new SQLFieldNameTranspiler().transpile(fieldName, traversal).getCode();
    return `${field} ${this.sqlOperator(operator.concatTokens())} '" + ${code} + "'`;
  }

  private sqlOperator(op: string): string {
    if (op.toUpperCase() === "EQ") {
      return "=";
    } else if (op.toUpperCase() === "NE") {
      return "<>";
    }
    return op;
  }

}
